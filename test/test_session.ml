open! Core
open! Async
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

(* Deterministic ids: a counter in every byte. *)
let counter_random () =
  let n = ref 0 in
  fun len ->
    incr n;
    String.init len ~f:(fun _ -> Char.of_int_exn (!n land 0xff))
;;

let secret c = String.make 32 c

let setup
  ?is_revoked
  ?(keyring = Session.Keyring.create ~current:("k1", secret 'a') ())
  ()
  =
  let manual = Clock.Manual.create ~start:t0 in
  let session =
    Session.create
      ?is_revoked
      ~ttl:(Time_ns.Span.of_hr 1.)
      ~max_lifetime:(Time_ns.Span.of_hr 3.)
      ~random:(counter_random ())
      ~keyring
      ~clock:(Clock.Manual.clock manual)
      ()
  in
  manual, session
;;

let show_result r =
  print_s
    [%sexp
      (Result.map r ~f:(fun (c : Session.Claims.t) -> c.subject, c.token_id)
       : (string * string, Session.Error.t) Result.t)]
;;

let%expect_test "issue and verify" =
  let manual, session = setup () in
  let token, claims = Session.issue session ~subject:"user-1" in
  print_endline token;
  print_s [%sexp (claims : Session.Claims.t)];
  let%bind r = Session.verify session token in
  show_result r;
  Clock.Manual.advance manual (Time_ns.Span.of_hr 1.);
  let%bind r = Session.verify session token in
  show_result r;
  [%expect
    {|
    v1.k1.IDAxMDEwMTAxMDEwMTAxMDEwMTAxMDEwMTAxMDEwMTAxIDAyMDIwMjAyMDIwMjAyMDIwMjAyMDIwMjAyMDIwMjAyBnVzZXItMfwAoLn6w1neGPwAoLn6w1neGPwAQHIrCl3eGA.NRr-QBEkLnCUCm5-txruU4D2eOPVi3r9VjroP4I6OhI
    ((session_id 01010101010101010101010101010101)
     (token_id 02020202020202020202020202020202) (subject user-1)
     (started_at "2026-10-14 09:00:00Z") (issued_at "2026-10-14 09:00:00Z")
     (expires_at "2026-10-14 10:00:00Z"))
    (Ok (user-1 02020202020202020202020202020202))
    (Error Expired)
    |}];
  return ()
;;

let%expect_test "tampering" =
  let _, session = setup () in
  let token, _ = Session.issue session ~subject:"user-1" in
  let parts = String.split token ~on:'.' in
  let with_part i s =
    List.mapi parts ~f:(fun j p -> if i = j then s else p) |> String.concat ~sep:"."
  in
  let _, other, _ =
    setup ~keyring:(Session.Keyring.create ~current:("k1", secret 'b') ()) ()
    |> fun (m, s) -> m, s, ()
  in
  let forged, _ = Session.issue other ~subject:"admin" in
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ "garbage"
      ; with_part 0 "v2"
      ; with_part 1 "k9"
      ; with_part 3 "AAAA"
      ; with_part 3 "!!"
      ; forged
      ]
      ~f:(fun token ->
        let%map r = Session.verify session token in
        show_result r)
  in
  [%expect
    {|
    (Error Malformed)
    (Error Malformed)
    (Error Unknown_key)
    (Error Bad_signature)
    (Error Malformed)
    (Error Bad_signature)
    |}];
  return ()
;;

let%expect_test "key rotation keeps old tokens valid" =
  let manual = Clock.Manual.create ~start:t0 in
  let clock = Clock.Manual.clock manual in
  let old_session =
    Session.create
      ~keyring:(Session.Keyring.create ~current:("k1", secret 'a') ())
      ~clock
      ()
  in
  let token, _ = Session.issue old_session ~subject:"user-1" in
  let new_session =
    Session.create
      ~keyring:
        (Session.Keyring.create
           ~current:("k2", secret 'b')
           ~previous:[ "k1", secret 'a' ]
           ())
      ~clock
      ()
  in
  let%bind r = Session.verify new_session token in
  print_s [%sexp (Result.is_ok r : bool)];
  let fresh, _ = Session.issue new_session ~subject:"user-1" in
  print_endline (List.nth_exn (String.split fresh ~on:'.') 1);
  [%expect {|
    true
    k2
    |}];
  return ()
;;

let%expect_test "token rotation and the maximum lifetime" =
  let manual, session = setup () in
  let token, first = Session.issue session ~subject:"user-1" in
  let rec go token n =
    if n = 0
    then return ()
    else (
      Clock.Manual.advance manual (Time_ns.Span.of_min 50.);
      match%bind Session.rotate session token with
      | Error e ->
        print_s [%sexp (e : Session.Error.t)];
        return ()
      | Ok (token, c) ->
        print_s
          [%sexp
            { same_session = (String.equal c.session_id first.session_id : bool)
            ; token_id = (c.token_id : string)
            ; expires_at = (c.expires_at : Time_ns.Alternate_sexp.t)
            }];
        go token (n - 1))
  in
  let%bind () = go token 5 in
  [%expect
    {|
    ((same_session true) (token_id 03030303030303030303030303030303)
     (expires_at "2026-10-14 10:50:00Z"))
    ((same_session true) (token_id 04040404040404040404040404040404)
     (expires_at "2026-10-14 11:40:00Z"))
    ((same_session true) (token_id 05050505050505050505050505050505)
     (expires_at "2026-10-14 12:00:00Z"))
    Expired
    |}];
  return ()
;;

let%expect_test "revocation hook" =
  let revoked = String.Hash_set.create () in
  let _, session =
    setup ~is_revoked:(fun c -> return (Hash_set.mem revoked c.session_id)) ()
  in
  let token, claims = Session.issue session ~subject:"user-1" in
  Hash_set.add revoked claims.session_id;
  let%bind r = Session.verify session token in
  show_result r;
  print_s
    [%sexp
      (Session.Error.to_problem Revoked |> Problem.to_yojson |> Yojson.Safe.to_string
       : string)];
  [%expect
    {|
    (Error Revoked)
    "{\"type\":\"about:blank\",\"title\":\"Unauthorized\",\"status\":401,\"code\":\"session_invalid\"}"
    |}];
  return ()
;;

let%expect_test "authorization header" =
  List.iter [ "Bearer abc"; "bearer  abc "; "Basic abc"; "Bearer "; "abc" ] ~f:(fun h ->
    print_s [%sexp (h : string), (Session.of_authorization_header h : string option)]);
  [%expect
    {|
    ("Bearer abc" (abc))
    ("bearer  abc " (abc))
    ("Basic abc" ())
    ("Bearer " ())
    (abc ())
    |}];
  return ()
;;

let%expect_test "keyring validation" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Session.Keyring.create ~current:("k1", "short") ());
  Expect_test_helpers_core.require_does_raise (fun () ->
    Session.Keyring.create ~current:("k.1", secret 'a') ());
  Expect_test_helpers_core.require_does_raise (fun () ->
    Session.Keyring.create ~current:("k1", secret 'a') ~previous:[ "k1", secret 'b' ] ());
  [%expect
    {|
    ("Session.Keyring: secret shorter than 32 bytes" (id k1))
    ("Session.Keyring: invalid key id" (id k.1))
    ("Session.Keyring: repeated key id" (id k1))
    |}];
  return ()
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Session.Claims.Stable.V1.t];
  [%expect {| 77962df9f900bab109d1866e780e70b3 |}];
  return ()
;;
