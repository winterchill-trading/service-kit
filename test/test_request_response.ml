open! Core
open Service_kit

let%expect_test "request parts" =
  let r =
    Request.create
      `POST
      "/accounts/ab%2Fc//statement/?limit=5&limit=6"
      ~headers:[ "Content-Type", "application/json" ]
      ~body:{|{"x": 1}|}
  in
  print_s
    [%sexp
      { path : string list = r.path
      ; limit : string option = Request.query r "limit"
      ; content_type : string option = Request.header r "content-type"
      ; json : string =
          Request.json r |> Result.ok |> Option.value_exn |> Yojson.Safe.to_string
      }];
  [%expect
    {|
    ((path (accounts ab/c statement)) (limit (5))
     (content_type (application/json)) (json "{\"x\":1}"))
    |}]
;;

let%expect_test "bodies" =
  let show r =
    print_s
      [%sexp
        (Result.map_error r ~f:(fun (p : Problem.t) -> p.code) : (int, string) Result.t)]
  in
  let decode body =
    Request.decode (Request.create `POST "/" ~body) (function
      | `Assoc [ ("n", `Int n) ] -> n
      | _ -> failwith "bad shape")
  in
  show (decode {|{"n": 3}|});
  show (decode {|{"n": "3"}|});
  show (decode "{");
  print_s
    [%sexp (Request.json (Request.create `POST "/" ~body:"  ") |> Result.is_ok : bool)];
  [%expect
    {|
    (Ok 3)
    (Error invalid_body)
    (Error invalid_json)
    true
    |}]
;;

let%expect_test "context" =
  let key = Univ_map.Key.create ~name:"user" String.sexp_of_t in
  let r = Request.create `GET "/" in
  print_s [%sexp (Request.find r key : string option)];
  print_s [%sexp (Request.find (Request.set r key "u1") key : string option)];
  [%expect {|
    ()
    (u1)
    |}]
;;

let%expect_test "responses" =
  print_s
    [%sexp (Response.json ~status:`Created (`Assoc [ "id", `String "a1" ]) : Response.t)];
  let p =
    Response.problem (Problem.too_many_requests ~retry_after:(Time_ns.Span.of_sec 3.))
  in
  print_s [%sexp (p : Response.t)];
  print_s [%sexp (Response.header p "Retry-After" : string option)];
  print_s
    [%sexp
      (Response.with_header (Response.empty `No_content) "x-a" "1"
       |> fun r -> Response.with_header r "X-A" "2"
       : Response.t)];
  [%expect
    {|
    ((status Created) (headers ((content-type application/json)))
     (body "{\"id\":\"a1\"}"))
    ((status Too_many_requests)
     (headers ((content-type application/problem+json) (retry-after 3)))
     (body
      "{\"type\":\"about:blank\",\"title\":\"Too Many Requests\",\"status\":429,\"code\":\"rate_limited\",\"params\":{\"retry_after_seconds\":3}}"))
    (3)
    ((status No_content) (headers ((X-A 2))) (body ""))
    |}]
;;
