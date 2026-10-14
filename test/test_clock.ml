open! Core
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

let%expect_test "manual clock moves only when told" =
  let manual = Clock.Manual.create ~start:t0 in
  let clock = Clock.Manual.clock manual in
  print_s [%sexp (Clock.now clock : Time_ns.Alternate_sexp.t)];
  Clock.Manual.advance manual (Time_ns.Span.of_sec 90.);
  print_s [%sexp (Clock.now clock : Time_ns.Alternate_sexp.t)];
  [%expect {|
    "2026-10-14 09:00:00Z"
    "2026-10-14 09:01:30Z"
    |}];
  Expect_test_helpers_core.require_does_raise (fun () -> Clock.Manual.set manual t0);
  [%expect
    {|
    ("Clock.Manual.set: time moves backwards"
      (time "2026-10-14 09:00:00Z")
      (now  "2026-10-14 09:01:30Z"))
    |}]
;;

let%expect_test "of_fun" =
  let clock = Clock.of_fun (fun () -> t0) in
  print_s [%sexp (Time_ns.equal (Clock.now clock) t0 : bool)];
  [%expect {| true |}]
;;
