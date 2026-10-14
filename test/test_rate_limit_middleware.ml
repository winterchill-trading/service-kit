open! Core
open! Async
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

let%expect_test "middleware" =
  let manual = Clock.Manual.create ~start:t0 in
  let limits =
    Rate_limit.create
      (Rate_limit.Config.create ~burst:2 ~refill_every:(Time_ns.Span.of_sec 10.))
  in
  let app =
    Rate_limit.middleware limits ~clock:(Clock.Manual.clock manual) (fun _ ->
      return (Response.text "ok"))
  in
  let call remote =
    let%map r = app (Request.create `GET "/" ~remote) in
    printf
      "%s %d %s\n"
      remote
      (Response.status_code r)
      (Option.value (Response.header r "retry-after") ~default:"-")
  in
  let%bind () = call "10.0.0.1" in
  let%bind () = call "10.0.0.1" in
  let%bind () = call "10.0.0.1" in
  let%bind () = call "10.0.0.2" in
  Clock.Manual.advance manual (Time_ns.Span.of_sec 10.);
  let%bind () = call "10.0.0.1" in
  [%expect
    {|
    10.0.0.1 200 -
    10.0.0.1 200 -
    10.0.0.1 429 10
    10.0.0.2 200 -
    10.0.0.1 200 -
    |}];
  return ()
;;
