open! Core
open Service_kit

let%expect_test "json encoding" =
  let t = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00.5Z" in
  let params =
    [ "name", Param.String "Ada"
    ; "count", Int 3
    ; "final", Bool false
    ; "amount", Param.decimal "-12.50" |> ok_exn
    ; "at", Time t
    ; "tags", List [ String "a"; String "b" ]
    ]
  in
  print_endline (Yojson.Safe.to_string (Param.params_to_yojson params));
  [%expect
    {| {"name":"Ada","count":3,"final":false,"amount":"-12.50","at":"2026-10-14T09:00:00.500000000Z","tags":["a","b"]} |}]
;;

let%expect_test "decimal validation" =
  List.iter [ "1"; "-0.5"; "12.50"; "1."; ".5"; "1e3"; "--1"; "" ] ~f:(fun s ->
    print_s [%sexp (s : string), (Result.is_ok (Param.decimal s) : bool)]);
  [%expect
    {|
    (1 true)
    (-0.5 true)
    (12.50 true)
    (1. false)
    (.5 false)
    (1e3 false)
    (--1 false)
    ("" false)
    |}]
;;

let%expect_test "kind mismatch" =
  print_s [%sexp (Param.of_yojson Int (`String "3") : Param.t Or_error.t)];
  print_s
    [%sexp
      (Param.of_yojson (List Decimal) (`List [ `String "1.5"; `String "x" ])
       : Param.t Or_error.t)];
  [%expect
    {|
    (Error ("Param.of_yojson: kind mismatch" (expected int) (got "\"3\"")))
    (Error ("Param.decimal: not a decimal" (s x)))
    |}]
;;

let%test_unit "json round trip" =
  Quickcheck.test Gens.homogeneous ~sexp_of:Param.sexp_of_t ~f:(fun p ->
    [%test_result: Param.t Or_error.t]
      (Param.of_yojson (Gens.kind_of p) (Param.to_yojson p))
      ~expect:(Ok p))
;;

let%expect_test "names" =
  List.iter
    [ "amount"; "max_2"; "Amount"; "2fa"; "a-b"; ""; String.make 65 'a' ]
    ~f:(fun s -> print_s [%sexp (s : string), (Param.is_valid_name s : bool)]);
  [%expect
    {|
    (amount true)
    (max_2 true)
    (Amount false)
    (2fa false)
    (a-b false)
    ("" false)
    (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa false)
    |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Param.Stable.V1.t];
  print_endline [%bin_digest: Param.Stable.Kind.V1.t];
  [%expect
    {|
    0ed6526cd51173292462a5016c15538d
    619ec88bce38cf3b12a87a415a1782a8
    |}]
;;
