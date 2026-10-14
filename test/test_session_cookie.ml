open! Core
open! Async
open Service_kit
module Sessions = Session.Server_side (Session.Memory_store)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

(* Deterministic tokens and ids: a counter in every byte. *)
let counter_random () =
  let n = ref 0 in
  fun len ->
    incr n;
    String.init len ~f:(fun _ -> Char.of_int_exn (!n land 0xff))
;;

let setup ?cookie () =
  let manual = Clock.Manual.create ~start:t0 in
  let store = Session.Memory_store.create () in
  let sessions =
    Sessions.create
      ?cookie
      ~ttl:(Time_ns.Span.of_hr 1.)
      ~max_lifetime:(Time_ns.Span.of_hr 3.)
      ~random:(counter_random ())
      ~store
      ~clock:(Clock.Manual.clock manual)
      ()
  in
  manual, store, sessions
;;

let show_verdict r =
  print_s
    [%sexp
      (Or_error.map
         r
         ~f:(Result.map ~f:(fun (c : Session.Claims.t) -> c.subject, c.token_id))
       : (string * string, Session.Error.t) Result.t Or_error.t)]
;;

let%expect_test "cookie attributes" =
  let show c =
    let name, value =
      Session.Cookie.set c ~value:"tok" ~max_age:(Time_ns.Span.of_sec 90.5)
    in
    print_endline (name ^ ": " ^ value)
  in
  show (Session.Cookie.create ());
  show
    (Session.Cookie.create
       ~name:"sid"
       ~secure:false
       ~http_only:false
       ~same_site:Strict
       ~path:"/app"
       ~domain:"example.com"
       ());
  let name, value = Session.Cookie.clear (Session.Cookie.create ()) in
  print_endline (name ^ ": " ^ value);
  [%expect
    {|
    set-cookie: __Host-session=tok; Path=/; Max-Age=91; Secure; HttpOnly; SameSite=Lax
    set-cookie: sid=tok; Path=/app; Domain=example.com; Max-Age=91; SameSite=Strict
    set-cookie: __Host-session=; Path=/; Max-Age=0; Secure; HttpOnly; SameSite=Lax
    |}];
  return ()
;;

let%expect_test "cookie settings browsers would drop are refused" =
  let try_ f =
    match f () with
    | (_ : Session.Cookie.t) -> print_endline "ok"
    | exception exn -> print_s [%sexp (exn : exn)]
  in
  try_ (fun () -> Session.Cookie.create ~secure:false ());
  try_ (fun () -> Session.Cookie.create ~domain:"example.com" ());
  try_ (fun () -> Session.Cookie.create ~name:"__Secure-s" ~secure:false ());
  try_ (fun () ->
    Session.Cookie.create ~name:"s" ~secure:false ~same_site:No_restriction ());
  try_ (fun () -> Session.Cookie.create ~name:"bad;name" ());
  try_ (fun () -> Session.Cookie.create ~name:"s" ~path:"app" ());
  try_ (fun () -> Session.Cookie.create ~name:"__Secure-s" ~domain:"example.com" ());
  [%expect
    {|
    ("Session.Cookie.create: a __Host- cookie needs Secure, Path=/ and no Domain"
     __Host-session)
    ("Session.Cookie.create: a __Host- cookie needs Secure, Path=/ and no Domain"
     __Host-session)
    ("Session.Cookie.create: a __Secure- cookie needs Secure" __Secure-s)
    ("Session.Cookie.create: SameSite=None needs Secure" s)
    ("Session.Cookie.create: invalid cookie name" "bad;name")
    ("Session.Cookie.create: path must start with /" s)
    ok
    |}];
  return ()
;;

let%expect_test "parsing Cookie headers" =
  print_s
    [%sexp
      (Session.Cookie.parse_header {|a=1; b="two"; flag; c = 3 ;d=|}
       : (string * string) list)];
  let cookie = Session.Cookie.create ~name:"sid" ~secure:false () in
  let find headers = Session.Cookie.find cookie (Request.create `GET "/" ~headers) in
  print_s
    [%sexp
      [ (find [ "cookie", "x=1; sid=abc" ] : string option)
      ; (find [ "cookie", "sid=; x=1"; "cookie", "sid=def" ] : string option)
      ; (find [ "cookie", "sidx=1" ] : string option)
      ; (find [] : string option)
      ]];
  [%expect {|
    ((a 1) (b two) (c 3) (d ""))
    ((abc) (def) () ())
    |}];
  return ()
;;

let%expect_test "issue, verify, rotate, revoke" =
  let manual, store, sessions = setup () in
  let%bind issued = Sessions.issue sessions ~subject:"cust-1" in
  let token, claims = Or_error.ok_exn issued in
  print_s [%sexp (String.length token : int), (claims.subject : string)];
  (* The store is keyed by the token's hash, never the token. *)
  let%bind found = Session.Memory_store.find store ~token_hash:token in
  print_s [%sexp (Or_error.ok_exn found |> Option.is_some : bool)];
  let%bind () = Sessions.verify sessions token >>| show_verdict in
  let%bind () = Sessions.verify sessions "not-a-token" >>| show_verdict in
  let%bind () =
    Sessions.verify sessions (Codec.base64url_encode (String.make 32 'z'))
    >>| show_verdict
  in
  Clock.Manual.advance manual (Time_ns.Span.of_min 30.);
  let%bind rotated = Sessions.rotate sessions token in
  let token2, claims2 = Or_error.ok_exn rotated |> Result.ok |> Option.value_exn in
  print_s
    [%sexp
      (String.equal claims.session_id claims2.session_id : bool)
      , (Time_ns.diff claims2.expires_at claims.expires_at : Time_ns.Span.t)];
  let%bind () = Sessions.verify sessions token >>| show_verdict in
  let%bind () = Sessions.verify sessions token2 >>| show_verdict in
  let%bind () = Sessions.revoke sessions token2 >>| Or_error.ok_exn in
  let%bind () = Sessions.verify sessions token2 >>| show_verdict in
  [%expect
    {|
    (43 cust-1)
    false
    (Ok (Ok (cust-1 01010101010101010101010101010101)))
    (Ok (Error Malformed))
    (Ok (Error Revoked))
    (true 30m)
    (Ok (Error Revoked))
    (Ok (Ok (cust-1 04040404040404040404040404040404)))
    (Ok (Error Revoked))
    |}];
  return ()
;;

let%expect_test "expiry, maximum lifetime, log out everywhere, purge" =
  let manual, store, sessions = setup () in
  let%bind issued = Sessions.issue sessions ~subject:"cust-1" in
  let token, claims = Or_error.ok_exn issued in
  let rec rotate_until_refused token n =
    Clock.Manual.advance manual (Time_ns.Span.of_min 50.);
    match%bind Sessions.rotate sessions token with
    | Ok (Ok (token, _)) -> rotate_until_refused token (n + 1)
    | Ok (Error e) -> return (n, e)
    | Error e -> Error.raise e
  in
  let%bind n, refusal = rotate_until_refused token 0 in
  print_s [%sexp (n : int), (refusal : Session.Error.t)];
  let%bind second = Sessions.issue sessions ~subject:"cust-1" in
  let token2, _ = Or_error.ok_exn second in
  let%bind removed = Sessions.revoke_session sessions ~session_id:claims.session_id in
  print_s [%sexp (removed : int Or_error.t)];
  Clock.Manual.advance manual (Time_ns.Span.of_hr 2.);
  let%bind () = Sessions.verify sessions token2 >>| show_verdict in
  let%bind purged = Sessions.purge sessions in
  print_s [%sexp (purged : int Or_error.t), (Session.Memory_store.length store : int)];
  [%expect {|
    (3 Expired)
    (Ok 1)
    (Ok (Error Expired))
    ((Ok 1) 0)
    |}];
  return ()
;;

let%expect_test "middleware: cookie, optional bearer, refusals" =
  let _, _, sessions =
    setup ~cookie:(Session.Cookie.create ~name:"sid" ~secure:false ()) ()
  in
  let%bind issued = Sessions.issue sessions ~subject:"cust-1" in
  let token, claims = Or_error.ok_exn issued in
  let set_name, set_value = Sessions.set_cookie sessions (token, claims) in
  print_endline
    (set_name ^ ": " ^ String.substr_replace_all set_value ~pattern:token ~with_:"<token>");
  let handler req =
    return
      (Response.text
         (Option.value_map (Session.claims req) ~default:"anonymous" ~f:(fun c ->
            c.subject)))
  in
  let run ?optional ?bearer headers =
    let%map r =
      Sessions.middleware
        ?optional
        ?bearer
        sessions
        handler
        (Request.create `GET "/" ~headers)
    in
    print_s
      [%sexp
        (Response.status_code r : int)
        , (if Response.status_code r = 200 then r.body else "" : string)
        , (Response.header r "www-authenticate" : string option)
        , (Response.header r "set-cookie" : string option)]
  in
  let%bind () = run [ "cookie", "sid=" ^ token ] in
  let%bind () = run [] in
  let%bind () = run ~optional:true [] in
  let%bind () = run [ "cookie", "sid=forged" ] in
  let%bind () = run [ "authorization", "Bearer " ^ token ] in
  let%bind () = run ~bearer:true [ "authorization", "Bearer " ^ token ] in
  let%bind () = run ~bearer:true [ "authorization", "Basic abc" ] in
  [%expect
    {|
    set-cookie: sid=<token>; Path=/; Max-Age=3600; HttpOnly; SameSite=Lax
    (200 cust-1 () ())
    (401 "" () ())
    (200 anonymous () ())
    (401 "" () ("sid=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax"))
    (401 "" () ())
    (200 cust-1 () ())
    (401 "" (Bearer) ())
    |}];
  return ()
;;

let%expect_test "a failing store answers 503" =
  let module Down = struct
    include Session.Memory_store

    let find _ ~token_hash:_ = return (Or_error.error_string "database down")
  end
  in
  let module S = Session.Server_side (Down) in
  let sessions =
    S.create
      ~cookie:(Session.Cookie.create ~name:"sid" ~secure:false ())
      ~store:(Down.create ())
      ~clock:(Clock.Manual.clock (Clock.Manual.create ~start:t0))
      ()
  in
  let%bind r =
    S.middleware
      sessions
      (fun _ -> return (Response.text "in"))
      (Request.create
         `GET
         "/"
         ~headers:[ "cookie", "sid=" ^ Codec.base64url_encode (String.make 32 'a') ])
  in
  print_endline r.body;
  [%expect
    {| {"type":"about:blank","title":"Service Unavailable","status":503,"code":"session_unavailable"} |}];
  return ()
;;
