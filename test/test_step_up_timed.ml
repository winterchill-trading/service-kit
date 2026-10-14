open! Core
open! Async
open Service_kit

module Purpose = struct
  type t = Send_payment [@@deriving sexp, compare, equal, enumerate]

  let to_string Send_payment = "send_payment"
  let of_string s = List.find all ~f:(fun p -> String.equal (to_string p) s)
end

module Grants = Step_up.Make (Purpose) (Step_up.Memory_spent)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

(* Wraps the memory store and prints the time each call is given, as a database store
   would see it. *)
module Recording = struct
  type t = Step_up.Memory_bindings.t

  let bind t ~grant_id ~binding ~until ~now =
    print_s
      [%message
        "bind"
          ~at:(Time_ns.diff now t0 : Time_ns.Span.t)
          ~until:(Time_ns.diff until t0 : Time_ns.Span.t)];
    Step_up.Memory_bindings.Timed.bind t ~grant_id ~binding ~until ~now
  ;;
end

let setup () =
  let manual = Clock.Manual.create ~start:t0 in
  let clock = Clock.Manual.clock manual in
  (* The store's own clock stays at [t0]: only the [now] passed in may move it on. *)
  let frozen = Clock.Manual.clock (Clock.Manual.create ~start:t0) in
  let bindings = Step_up.Memory_bindings.create ~clock:frozen () in
  let grants =
    Grants.create
      ~mode:(Step_up.Mode.bind_on_first_use_timed (module Recording) bindings)
      ~key:(String.make 32 'k')
      ~spent:(Step_up.Memory_spent.create ~clock ())
      ~clock
      ()
  in
  manual, bindings, grants
;;

let binding idempotency_key =
  { Step_up.Binding.operation = "POST /payments"; idempotency_key }
;;

let show r =
  print_s
    [%sexp
      (Result.map r ~f:(fun (_ : Grants.Grant.t) -> "ok")
       : (string, Step_up.Error.t) Result.t)]
;;

let%expect_test "timed bindings receive the checker's clock" =
  let manual, bindings, grants = setup () in
  let token, _ = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  let use k =
    Grants.use grants token ~session_id:"s1" ~purpose:Send_payment ~binding:(binding k)
  in
  let%bind r = use "k1" in
  show r;
  Clock.Manual.advance manual (Time_ns.Span.of_min 2.);
  let%bind r = use "k1" in
  show r;
  let%bind r = use "k2" in
  show r;
  print_s [%sexp (Step_up.Memory_bindings.length bindings : int)];
  [%expect
    {|
    (bind (at 0s) (until 5m))
    (Ok ok)
    (bind (at 2m) (until 5m))
    (Ok ok)
    (bind (at 2m) (until 5m))
    (Error Already_used)
    1
    |}];
  return ()
;;

let%expect_test "Memory_bindings.Timed drops lapsed entries by the time it is given" =
  let store =
    Step_up.Memory_bindings.create
      ~clock:(Clock.Manual.clock (Clock.Manual.create ~start:t0))
      ()
  in
  let bind ~grant_id ~at =
    Step_up.Memory_bindings.Timed.bind
      store
      ~grant_id
      ~binding:(binding "k")
      ~until:(Time_ns.add t0 (Time_ns.Span.of_min 5.))
      ~now:(Time_ns.add t0 at)
  in
  let%bind (_ : _ Or_error.t) = bind ~grant_id:"g1" ~at:Time_ns.Span.zero in
  print_s [%sexp (Step_up.Memory_bindings.length store : int)];
  let%bind (_ : _ Or_error.t) = bind ~grant_id:"g2" ~at:(Time_ns.Span.of_min 6.) in
  print_s [%sexp (Step_up.Memory_bindings.length store : int)];
  [%expect {|
    1
    1
    |}];
  return ()
;;
