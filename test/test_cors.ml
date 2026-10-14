open! Core
open! Async
open Service_kit

let app (_ : Request.t) = return (Response.text "ok")

let show cors ?(headers = []) meth =
  let%map resp = Cors.middleware cors app (Request.create meth "/x" ~headers) in
  printf "%d %s\n" (Response.status_code resp) resp.body;
  List.iter resp.headers ~f:(fun (n, v) ->
    if not (String.equal n "content-type") then printf "  %s: %s\n" n v)
;;

let%expect_test "listed origins" =
  let cors = Cors.create ~origins:(`Only [ "https://app.example.com" ]) () in
  let preflight origin = [ "origin", origin; "access-control-request-method", "POST" ] in
  let%bind () = show cors `OPTIONS ~headers:(preflight "https://app.example.com") in
  let%bind () = show cors `OPTIONS ~headers:(preflight "https://evil.example") in
  let%bind () = show cors `GET ~headers:[ "origin", "https://app.example.com" ] in
  let%bind () = show cors `GET ~headers:[ "origin", "https://evil.example" ] in
  let%bind () = show cors `GET in
  (* OPTIONS without the preflight header reaches the application. *)
  let%bind () = show cors `OPTIONS ~headers:[ "origin", "https://app.example.com" ] in
  [%expect
    {|
    204
      access-control-allow-origin: https://app.example.com
      vary: Origin
      access-control-allow-methods: GET, POST, PUT, PATCH, DELETE
      access-control-allow-headers: authorization, content-type, idempotency-key, x-request-id
      access-control-max-age: 600
    204
      vary: Origin
    200 ok
      access-control-allow-origin: https://app.example.com
      vary: Origin
      access-control-expose-headers: x-request-id, retry-after, idempotent-replayed
    200 ok
      vary: Origin
    200 ok
    200 ok
      access-control-allow-origin: https://app.example.com
      vary: Origin
      access-control-expose-headers: x-request-id, retry-after, idempotent-replayed
    |}];
  return ()
;;

let%expect_test "any origin and credentials" =
  let cors = Cors.create ~origins:`Any ~expose_headers:[] () in
  let%bind () = show cors `GET ~headers:[ "origin", "https://x.example" ] in
  let cors =
    Cors.create ~credentials:true ~origins:(`Only [ "https://app.example.com" ]) ()
  in
  let%bind () = show cors `GET ~headers:[ "origin", "https://app.example.com" ] in
  Expect_test_helpers_core.require_does_raise (fun () ->
    Cors.create ~credentials:true ~origins:`Any ());
  [%expect
    {|
    200 ok
      access-control-allow-origin: *
      vary: Origin
    200 ok
      access-control-allow-origin: https://app.example.com
      vary: Origin
      access-control-allow-credentials: true
      access-control-expose-headers: x-request-id, retry-after, idempotent-replayed
    "Cors.create: credentials with any origin"
    |}];
  return ()
;;
