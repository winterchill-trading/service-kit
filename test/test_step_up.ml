open! Core
open! Async
open Service_kit

module Purpose = struct
  type t =
    | Send_payment
    | Reveal_card
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
    | Send_payment -> "send_payment"
    | Reveal_card -> "reveal_card"
  ;;

  let of_string s = List.find all ~f:(fun p -> String.equal (to_string p) s)
end

module Grants = Step_up.Make (Purpose) (Step_up.Memory_spent)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

let setup () =
  let manual = Clock.Manual.create ~start:t0 in
  let clock = Clock.Manual.clock manual in
  let n = ref 0 in
  let random len =
    incr n;
    String.make len (Char.of_int_exn !n)
  in
  let spent = Step_up.Memory_spent.create ~clock () in
  let grants = Grants.create ~random ~key:(String.make 32 'k') ~spent ~clock () in
  manual, spent, grants
;;

let show r =
  print_s
    [%sexp
      (Result.map r ~f:(fun (g : Grants.Grant.t) -> g.grant_id)
       : (string, Step_up.Error.t) Result.t)]
;;

let%expect_test "consume once" =
  let _, spent, grants = setup () in
  let token, grant = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  print_s [%sexp (grant : Grants.Grant.t)];
  show (Grants.check grants token ~session_id:"s1" ~purpose:Send_payment);
  let%bind r = Grants.consume grants token ~session_id:"s1" ~purpose:Send_payment in
  show r;
  let%bind r = Grants.consume grants token ~session_id:"s1" ~purpose:Send_payment in
  show r;
  print_s [%sexp (Step_up.Memory_spent.length spent : int)];
  [%expect
    {|
    ((grant_id 01010101010101010101010101010101) (session_id s1)
     (purpose Send_payment) (issued_at "2026-10-14 09:00:00Z")
     (expires_at "2026-10-14 09:05:00Z"))
    (Ok 01010101010101010101010101010101)
    (Ok 01010101010101010101010101010101)
    (Error Already_used)
    1
    |}];
  return ()
;;

let%expect_test "bound to session, purpose and time" =
  let manual, spent, grants = setup () in
  let token, _ = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  show (Grants.check grants token ~session_id:"s2" ~purpose:Send_payment);
  show (Grants.check grants token ~session_id:"s1" ~purpose:Reveal_card);
  show (Grants.check grants (token ^ "x") ~session_id:"s1" ~purpose:Send_payment);
  show (Grants.check grants "su1.abc" ~session_id:"s1" ~purpose:Send_payment);
  let%bind _ = Grants.consume grants token ~session_id:"s1" ~purpose:Send_payment in
  Clock.Manual.advance manual (Time_ns.Span.of_min 5.);
  show (Grants.check grants token ~session_id:"s1" ~purpose:Send_payment);
  (* Consuming another grant drops the first one's entry, which has expired. *)
  let token2, _ = Grants.issue grants ~session_id:"s1" ~purpose:Reveal_card in
  let%bind r = Grants.consume grants token2 ~session_id:"s1" ~purpose:Reveal_card in
  show r;
  print_s [%sexp (Step_up.Memory_spent.length spent : int)];
  print_endline
    (Yojson.Safe.to_string (Problem.to_yojson (Step_up.Error.to_problem Expired)));
  [%expect
    {|
    (Error Wrong_session)
    (Error Wrong_purpose)
    (Error Bad_signature)
    (Error Malformed)
    (Error Expired)
    (Ok 02020202020202020202020202020202)
    1
    {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required"}
    |}];
  return ()
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Step_up.Stable.Claims.V1.t];
  [%expect {| bc5abdf35ff89f8851e9eba459ff6db4 |}];
  return ()
;;
