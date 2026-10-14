open! Core
open! Async
open Service_kit

let ok () = return (Ok ())
let fail () = return (Or_error.error_string "connection refused to db-1:5432")
let hang () = Deferred.never ()
let show resp = printf "%d %s\n" (Response.status_code resp) resp.Response.body

let%expect_test "readiness" =
  let short = Time_ns.Span.of_ms 20. in
  let case ?verbose checks =
    let%map resp = Health.ready (Health.create ?verbose checks) in
    show resp
  in
  let%bind () = case [ Health.Check.create "database" ok ] in
  let%bind () =
    case
      [ Health.Check.create "database" ok
      ; Health.Check.create "rates" ~critical:false ~timeout:short hang
      ]
  in
  let%bind () =
    case
      [ Health.Check.create "database" fail
      ; Health.Check.create "ledger" ~timeout:short hang
      ; Health.Check.create "raises" (fun () -> failwith "boom")
      ]
  in
  let%bind () = case ~verbose:true [ Health.Check.create "database" fail ] in
  [%expect
    {|
    200 {"status":"ok","checks":{"database":"ok"}}
    200 {"status":"degraded","checks":{"database":"ok","rates":"timeout"}}
    503 {"status":"unavailable","checks":{"database":"failed","ledger":"timeout","raises":"failed"}}
    503 {"status":"unavailable","checks":{"database":"failed: connection refused to db-1:5432"}}
    |}];
  return ()
;;

let%expect_test "routes and draining" =
  let health = Health.create [ Health.Check.create "database" ok ] in
  let router = Router.create (Health.routes health) in
  let get path = Router.handle router (Request.create `GET path) >>| show in
  let%bind () = get "/healthz" in
  let%bind () = get "/readyz" in
  Health.set_draining health true;
  let%bind () = get "/readyz" in
  let%bind () = get "/healthz" in
  [%expect
    {|
    200 {"status":"ok"}
    200 {"status":"ok","checks":{"database":"ok"}}
    503 {"status":"draining"}
    200 {"status":"ok"}
    |}];
  return ()
;;
