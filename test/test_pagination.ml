open! Core
open Service_kit

module Position = struct
  type t = int * string [@@deriving bin_io, sexp_of, compare, equal]
end

module Cursor = Pagination.Cursor (Position)

let key = String.make 32 'k'

let show_decode r =
  print_s
    [%sexp
      (Result.map_error r ~f:(fun (p : Problem.t) -> p.code)
       : (Position.t, string) Result.t)]
;;

let%expect_test "encode and decode" =
  let plain = Cursor.encode (42, "pay_1") in
  let signed = Cursor.encode ~key (42, "pay_1") in
  print_endline plain;
  print_endline signed;
  show_decode (Cursor.decode plain);
  show_decode (Cursor.decode ~key signed);
  show_decode (Cursor.decode ~key plain);
  show_decode (Cursor.decode ~key:(String.make 32 'x') signed);
  show_decode (Cursor.decode "");
  show_decode (Cursor.decode "not a cursor");
  show_decode (Cursor.decode (plain ^ "AA"));
  [%expect
    {|
    ASoFcGF5XzE
    ASoFcGF5XzF5-KjfGd1iJu0kOFhefMOd
    (Ok (42 pay_1))
    (Ok (42 pay_1))
    (Error invalid_cursor)
    (Error invalid_cursor)
    (Error invalid_cursor)
    (Error invalid_cursor)
    (Error invalid_cursor)
    |}]
;;

let%test_unit "round trip" =
  Quickcheck.test
    [%quickcheck.generator: int * string * bool]
    ~sexp_of:[%sexp_of: int * string * bool]
    ~f:(fun (n, s, signed) ->
      let key = if signed then Some key else None in
      match Cursor.decode ?key (Cursor.encode ?key (n, s)) with
      | Ok p -> [%test_result: Position.t] p ~expect:(n, s)
      | Error _ -> failwith "decode failed")
;;

let%expect_test "limit" =
  List.iter
    [ None; Some "10"; Some "0"; Some "201"; Some "abc"; Some " 200 " ]
    ~f:(fun s ->
      print_s
        [%sexp
          (s : string option)
          , (Result.map_error (Pagination.limit s) ~f:(fun p ->
               Problem.to_yojson p |> Yojson.Safe.to_string)
             : (int, string) Result.t)]);
  [%expect
    {|
    (() (Ok 50))
    ((10) (Ok 10))
    ((0)
     (Error
      "{\"type\":\"about:blank\",\"title\":\"Bad Request\",\"status\":400,\"code\":\"invalid_limit\",\"params\":{\"max\":200}}"))
    ((201)
     (Error
      "{\"type\":\"about:blank\",\"title\":\"Bad Request\",\"status\":400,\"code\":\"invalid_limit\",\"params\":{\"max\":200}}"))
    ((abc)
     (Error
      "{\"type\":\"about:blank\",\"title\":\"Bad Request\",\"status\":400,\"code\":\"invalid_limit\",\"params\":{\"max\":200}}"))
    ((" 200 ") (Ok 200))
    |}]
;;

let%expect_test "pages" =
  let rows = List.init 5 ~f:(fun i -> i, sprintf "r%d" i) in
  let cursor (n, _) = Int.to_string n in
  let show p =
    print_endline
      (Yojson.Safe.to_string (Pagination.Page.to_yojson (fun (_, s) -> `String s) p))
  in
  show (Pagination.Page.of_overfetch (List.take rows 3) ~limit:2 ~cursor);
  show (Pagination.Page.of_overfetch (List.take rows 2) ~limit:2 ~cursor);
  show (Pagination.Page.of_overfetch [] ~limit:2 ~cursor);
  [%expect
    {|
    {"items":["r0","r1"],"next_cursor":"1"}
    {"items":["r0","r1"],"next_cursor":null}
    {"items":[],"next_cursor":null}
    |}]
;;
