open! Core
open Service_kit

let show ?type_base p =
  print_endline (Yojson.Safe.to_string (Problem.to_yojson ?type_base p));
  if not (List.is_empty p.Problem.headers)
  then print_s [%sexp (p.headers : (string * string) list)]
;;

let%expect_test "documents" =
  show
    (Problem.conflict
       "insufficient_funds"
       ~params:[ "available", Param.Decimal "12.50"; "currency", String "EUR" ]);
  show ~type_base:"https://errors.example.com/" (Problem.not_found "account_not_found");
  show (Problem.with_instance Problem.internal "req-42");
  show (Problem.too_many_requests ~retry_after:(Time_ns.Span.of_ms 1500.));
  show (Problem.too_many_requests ~retry_after:Time_ns.Span.zero);
  show (Problem.method_not_allowed ~allowed:[ `GET; `POST ]);
  show (Problem.payload_too_large ~max_bytes:65_536);
  [%expect
    {|
    {"type":"about:blank","title":"Conflict","status":409,"code":"insufficient_funds","params":{"available":"12.50","currency":"EUR"}}
    {"type":"https://errors.example.com/account_not_found","title":"Not Found","status":404,"code":"account_not_found"}
    {"type":"about:blank","title":"Internal Server Error","status":500,"code":"internal_error","instance":"req-42"}
    {"type":"about:blank","title":"Too Many Requests","status":429,"code":"rate_limited","params":{"retry_after_seconds":2}}
    ((retry-after 2))
    {"type":"about:blank","title":"Too Many Requests","status":429,"code":"rate_limited","params":{"retry_after_seconds":1}}
    ((retry-after 1))
    {"type":"about:blank","title":"Method Not Allowed","status":405,"code":"method_not_allowed"}
    ((allow "GET, POST"))
    {"type":"about:blank","title":"Request Entity Too Large","status":413,"code":"payload_too_large","params":{"max_bytes":65536}}
    |}]
;;

let%expect_test "codes and param names are checked" =
  Expect_test_helpers_core.require_does_raise (fun () -> Problem.bad_request "Bad Thing");
  Expect_test_helpers_core.require_does_raise (fun () ->
    Problem.bad_request
      "invalid_field"
      ~params:[ "field", String "a"; "field", String "b" ]);
  Expect_test_helpers_core.require_does_raise (fun () ->
    Problem.bad_request "invalid_field" ~params:[ "Field", String "a" ]);
  [%expect
    {|
    ("Problem.create: invalid code" (code "Bad Thing"))
    ("Problem.create: repeated param"
      (code invalid_field)
      (name field))
    ("Problem.create: invalid param name"
      (code invalid_field)
      (name Field))
    |}]
;;

let%test_unit "of_yojson inverts to_yojson for untyped params" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind code = Gens.name
    and status =
      Quickcheck.Generator.of_list [ `Bad_request; `Conflict; `Not_found; `Forbidden ]
    and instance = Option.quickcheck_generator String.quickcheck_generator
    and values =
      List.gen_with_length
        3
        (Quickcheck.Generator.union
           [ (String.quickcheck_generator >>| fun s -> Param.String s)
           ; (Int.quickcheck_generator >>| fun i -> Param.Int i)
           ; (Bool.quickcheck_generator >>| fun b -> Param.Bool b)
           ])
    in
    let params = List.mapi values ~f:(fun i v -> sprintf "p%d" i, v) in
    return (Problem.create ?instance ~params status code)
  in
  Quickcheck.test gen ~sexp_of:Problem.sexp_of_t ~f:(fun p ->
    [%test_result: Problem.t Or_error.t]
      (Problem.of_yojson (Problem.to_yojson p))
      ~expect:(Ok p))
;;
