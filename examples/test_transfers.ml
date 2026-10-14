(* Drives the example over its router, the way a client would. *)

open! Core
open! Async
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

let setup () =
  let n = ref 0 in
  let random len =
    incr n;
    String.init len ~f:(fun i -> Char.of_int_exn ((!n + i) land 0xff))
  in
  let manual = Clock.Manual.create ~start:t0 in
  let service = Transfers.create ~random ~clock:(Clock.Manual.clock manual) () in
  Transfers.router service
;;

let call router ?(headers = []) ?(body = "") meth path =
  let%map r = Router.handle router (Request.create meth path ~headers ~body) in
  let json =
    try Some (Yojson.Safe.from_string r.body) with
    | _ -> None
  in
  Response.status_code r, json, r
;;

let str json name =
  match json with
  | Some (`Assoc l) ->
    (match List.Assoc.find l name ~equal:String.equal with
     | Some (`String s) -> s
     | _ -> "")
  | _ -> ""
;;

let show (status, json, (r : Response.t)) =
  printf
    "%d %s%s\n"
    status
    (Option.value_map json ~default:r.body ~f:Yojson.Safe.to_string)
    (if Option.is_some (Response.header r "idempotent-replayed")
     then " (replayed)"
     else "")
;;

let%expect_test "send money" =
  let router = setup () in
  let%bind _, json, _ = call router `POST "/sessions" ~body:{|{"user":"alice"}|} in
  let auth = "authorization", "Bearer " ^ str json "token" in
  let transfer ?(grant = []) ?(key = "k1") amount =
    call
      router
      `POST
      "/transfers"
      ~headers:([ auth; "idempotency-key", key ] @ grant)
      ~body:(sprintf {|{"to":"bob","amount":"%s"}|} amount)
  in
  (* No step-up yet. *)
  let%bind () = transfer "12.50" >>| show in
  let%bind () =
    call
      router
      `POST
      "/step-up"
      ~headers:[ auth ]
      ~body:{|{"purpose":"send_transfer","passcode":"1234"}|}
    >>| show
  in
  let step_up () =
    let%map _, json, _ =
      call
        router
        `POST
        "/step-up"
        ~headers:[ auth ]
        ~body:{|{"purpose":"send_transfer","passcode":"0000"}|}
    in
    [ "x-step-up-grant", str json "grant" ]
  in
  let%bind grant = step_up () in
  let%bind () = transfer ~grant "12.50" >>| show in
  (* The response was lost; the retry replays it without a new grant. *)
  let%bind () = transfer ~grant "12.50" >>| show in
  let%bind () = transfer ~grant "13.00" >>| show in
  let%bind grant = step_up () in
  let%bind () = transfer ~grant ~key:"k2" "500.00" >>| show in
  let%bind grant = step_up () in
  let%bind () = transfer ~grant ~key:"k3" "1.5" >>| show in
  let%bind () = call router `GET "/balance" ~headers:[ auth ] >>| show in
  [%expect
    {|
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required","params":{"purpose":"send_transfer"}}
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"invalid_passcode"}
    201 {"id":1,"message":{"code":"transfer_sent","params":{"amount":"12.50","to":"bob"}}}
    201 {"id":1,"message":{"code":"transfer_sent","params":{"amount":"12.50","to":"bob"}}} (replayed)
    409 {"type":"about:blank","title":"Conflict","status":409,"code":"idempotency_key_reused"}
    409 {"type":"about:blank","title":"Conflict","status":409,"code":"insufficient_funds","params":{"available":"87.50"}}
    400 {"type":"about:blank","title":"Bad Request","status":400,"code":"invalid_amount"}
    200 {"balance":"87.50"}
    |}];
  return ()
;;

let%expect_test "pages, health, metrics and the catalogue" =
  let router = setup () in
  let%bind _, json, _ = call router `POST "/sessions" ~body:{|{"user":"alice"}|} in
  let auth = "authorization", "Bearer " ^ str json "token" in
  let%bind () =
    Deferred.List.iter ~how:`Sequential [ "1.00"; "2.00"; "3.00" ] ~f:(fun amount ->
      let%bind _, g, _ =
        call
          router
          `POST
          "/step-up"
          ~headers:[ auth ]
          ~body:{|{"purpose":"send_transfer","passcode":"0000"}|}
      in
      let%map (_ : _) =
        call
          router
          `POST
          "/transfers"
          ~headers:[ auth; "x-step-up-grant", str g "grant" ]
          ~body:(sprintf {|{"to":"bob","amount":"%s"}|} amount)
      in
      ())
  in
  let%bind ((_, page1, _) as r) = call router `GET "/transfers" ~headers:[ auth ] in
  show r;
  let cursor =
    match page1 with
    | Some (`Assoc l) ->
      (match List.Assoc.find l "next_cursor" ~equal:String.equal with
       | Some (`String c) -> c
       | _ -> "")
    | _ -> ""
  in
  let%bind () =
    call router `GET ("/transfers?cursor=" ^ cursor) ~headers:[ auth ] >>| show
  in
  let%bind () = call router `GET "/transfers?cursor=bogus" ~headers:[ auth ] >>| show in
  let%bind () = call router `GET "/readyz" >>| show in
  let%bind _, _, metrics = call router `GET "/metrics" in
  String.split_lines metrics.body
  |> List.filter ~f:(String.is_prefix ~prefix:"http_requests_total")
  |> List.iter ~f:print_endline;
  print_s
    [%sexp
      (Message.Catalogue.check
         Transfers.catalogue
         ~codes:Transfers.codes
         ~locales:[ "en"; "de" ]
       : unit Or_error.t)];
  [%expect
    {|
    200 {"items":[{"id":3,"from":"alice","to":"bob","amount":"3.00"},{"id":2,"from":"alice","to":"bob","amount":"2.00"}],"next_cursor":"AQJU-bVuCGbqxblSMA6XHycM"}
    200 {"items":[{"id":1,"from":"alice","to":"bob","amount":"1.00"}],"next_cursor":null}
    400 {"type":"about:blank","title":"Bad Request","status":400,"code":"invalid_cursor"}
    200 {"status":"ok","checks":{"memory":"ok"}}
    http_requests_total{route="/readyz",method="GET",status="200"} 1
    http_requests_total{route="/sessions",method="POST",status="201"} 1
    http_requests_total{route="/step-up",method="POST",status="201"} 3
    http_requests_total{route="/transfers",method="GET",status="200"} 2
    http_requests_total{route="/transfers",method="GET",status="400"} 1
    http_requests_total{route="/transfers",method="POST",status="201"} 3
    (Ok ())
    |}];
  return ()
;;
