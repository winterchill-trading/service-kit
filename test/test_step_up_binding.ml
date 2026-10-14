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
  let bindings = Step_up.Memory_bindings.create ~clock () in
  let grants =
    Grants.create
      ~mode:(Step_up.Mode.bind_on_first_use (module Step_up.Memory_bindings) bindings)
      ~random
      ~key:(String.make 32 'k')
      ~spent:(Step_up.Memory_spent.create ~clock ())
      ~clock
      ()
  in
  manual, bindings, grants
;;

let show r =
  print_s
    [%sexp
      (Result.map r ~f:(fun (g : Grants.Grant.t) -> g.grant_id)
       : (string, Step_up.Error.t) Result.t)]
;;

let binding ?(operation = "POST /payments") idempotency_key =
  { Step_up.Binding.operation; idempotency_key }
;;

let%expect_test "retries of the first request reuse the grant" =
  let manual, bindings, grants = setup () in
  let token, _ = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  let use b = Grants.use grants token ~session_id:"s1" ~purpose:Send_payment ~binding:b in
  let%bind r = use (binding "k1") in
  show r;
  let%bind r = use (binding "k1") in
  show r;
  let%bind r = use (binding "k2") in
  show r;
  let%bind r = use (binding ~operation:"POST /transfers" "k1") in
  show r;
  print_s [%sexp (Step_up.Memory_bindings.length bindings : int)];
  (* Purpose, session and expiry are still checked before the binding. *)
  let%bind r =
    Grants.use grants token ~session_id:"s2" ~purpose:Send_payment ~binding:(binding "k1")
  in
  show r;
  Clock.Manual.advance manual (Time_ns.Span.of_min 5.);
  let%bind r = use (binding "k1") in
  show r;
  [%expect
    {|
    (Ok 01010101010101010101010101010101)
    (Ok 01010101010101010101010101010101)
    (Error Already_used)
    (Error Already_used)
    1
    (Error Wrong_session)
    (Error Expired)
    |}];
  return ()
;;

let%expect_test "consume in binding mode is still single use" =
  let _, _, grants = setup () in
  let token, _ = Grants.issue grants ~session_id:"s1" ~purpose:Reveal_card in
  let%bind r = Grants.consume grants token ~session_id:"s1" ~purpose:Reveal_card in
  show r;
  let%bind r = Grants.consume grants token ~session_id:"s1" ~purpose:Reveal_card in
  show r;
  let%bind r =
    Grants.use grants token ~session_id:"s1" ~purpose:Reveal_card ~binding:(binding "k1")
  in
  show r;
  [%expect
    {|
    (Ok 01010101010101010101010101010101)
    (Error Already_used)
    (Error Already_used)
    |}];
  return ()
;;

let%expect_test "use in consume-once mode ignores the binding" =
  let clock = Clock.Manual.clock (Clock.Manual.create ~start:t0) in
  let grants =
    Grants.create
      ~key:(String.make 32 'k')
      ~spent:(Step_up.Memory_spent.create ~clock ())
      ~clock
      ()
  in
  let token, _ = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  let use () =
    Grants.use grants token ~session_id:"s1" ~purpose:Send_payment ~binding:(binding "k1")
  in
  let%bind r = use () in
  print_s [%sexp (Result.is_ok r : bool)];
  let%bind r = use () in
  show r;
  [%expect {|
    true
    (Error Already_used)
    |}];
  return ()
;;

let%expect_test "middleware binds to method, route and idempotency key" =
  let _, _, grants = setup () in
  let router =
    Router.create
      Router.
        [ Route.post
            Path.(root / "payments")
            ~middleware:
              [ Grants.require grants ~purpose:Send_payment ~session_id:(fun _ ->
                  Some "s1")
              ]
            (fun _ -> return (Response.text "paid"))
        ; Route.post
            Path.(root / "transfers")
            ~middleware:
              [ Grants.require grants ~purpose:Send_payment ~session_id:(fun _ ->
                  Some "s1")
              ]
            (fun _ -> return (Response.text "moved"))
        ]
  in
  let call ?key path token =
    let headers =
      (Grants.header_name, token)
      :: Option.value_map key ~default:[] ~f:(fun k -> [ Idempotency.header_name, k ])
    in
    let%map r = Router.handle router (Request.create `POST path ~headers) in
    printf "%d %s\n" (Response.status_code r) r.body
  in
  let token, _ = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  let%bind () = call ~key:"k1" "/payments" token in
  let%bind () = call ~key:"k1" "/payments" token in
  let%bind () = call ~key:"k2" "/payments" token in
  let%bind () = call ~key:"k1" "/transfers" token in
  (* Without an idempotency key a grant cannot be retried. *)
  let token2, _ = Grants.issue grants ~session_id:"s1" ~purpose:Send_payment in
  let%bind () = call "/payments" token2 in
  let%bind () = call "/payments" token2 in
  [%expect
    {|
    200 paid
    200 paid
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required","params":{"purpose":"send_payment"}}
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required","params":{"purpose":"send_payment"}}
    200 paid
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required","params":{"purpose":"send_payment"}}
    |}];
  return ()
;;

let%expect_test "binding sexp" =
  print_s [%sexp (binding "k1" : Step_up.Binding.t)];
  [%expect {| ((operation "POST /payments") (idempotency_key k1)) |}];
  return ()
;;
