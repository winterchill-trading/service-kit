open! Core
open! Async
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

let run ?redact ?(headers = []) target =
  let manual = Clock.Manual.create ~start:t0 in
  let entries = Queue.create () in
  let n = ref 0 in
  let middleware =
    Request_log.middleware
      ?redact
      ~log_headers:[ "Authorization"; "User-Agent"; "X-Forwarded-For" ]
      ~random:(fun () ->
        incr n;
        sprintf "gen-%d" !n)
      ~clock:(Clock.Manual.clock manual)
      ~sink:(Queue.enqueue entries)
      ()
  in
  let app (req : Request.t) =
    Clock.Manual.advance manual (Time_ns.Span.of_ms 12.5);
    Request_log.add_field req "seen_id" req.request_id;
    return (Response.text "ok")
  in
  let%map resp =
    middleware
      app
      { (Request.create `GET target ~headers ~remote:"10.0.0.7") with route = "/things" }
  in
  print_s [%sexp (Response.header resp "x-request-id" : string option)];
  Queue.iter entries ~f:(fun e ->
    print_endline (Yojson.Safe.to_string (Request_log.Entry.to_yojson e)))
;;

let%expect_test "entry with generated id and redaction" =
  let%bind () =
    run
      "/things?token=abc&page=2&code=x"
      ~headers:[ "authorization", "Bearer secret"; "user-agent", "curl/8" ]
  in
  [%expect
    {|
    (gen-1)
    {"request_id":"gen-1","method":"GET","route":"/things","target":"/things?token=REDACTED&page=2&code=REDACTED","status":200,"duration_ms":12.5,"remote":"10.0.0.7","headers":{"authorization":"REDACTED","user-agent":"curl/8"},"fields":{"seen_id":"gen-1"}}
    |}];
  return ()
;;

let%expect_test "caller ids are kept when valid" =
  let%bind () = run "/things" ~headers:[ "x-request-id", "abc-123.x" ] in
  let%bind () = run "/things" ~headers:[ "x-request-id", "has space" ] in
  [%expect
    {|
    (abc-123.x)
    {"request_id":"abc-123.x","method":"GET","route":"/things","target":"/things","status":200,"duration_ms":12.5,"remote":"10.0.0.7","fields":{"seen_id":"abc-123.x"}}
    (gen-1)
    {"request_id":"gen-1","method":"GET","route":"/things","target":"/things","status":200,"duration_ms":12.5,"remote":"10.0.0.7","fields":{"seen_id":"gen-1"}}
    |}];
  return ()
;;

let%expect_test "custom redaction" =
  let redact =
    Request_log.Redact.create
      ~query:[ "iban" ]
      ~hook:(fun ~name v ->
        if String.equal name "x-forwarded-for" then String.prefix v 4 ^ "..." else v)
      ()
  in
  let%bind () =
    run ~redact "/things?iban=DE89&token=t" ~headers:[ "x-forwarded-for", "203.0.113.9" ]
  in
  [%expect
    {|
    (gen-1)
    {"request_id":"gen-1","method":"GET","route":"/things","target":"/things?iban=REDACTED&token=REDACTED","status":200,"duration_ms":12.5,"remote":"10.0.0.7","headers":{"x-forwarded-for":"203...."},"fields":{"seen_id":"gen-1"}}
    |}];
  return ()
;;
