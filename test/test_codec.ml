open! Core
open Service_kit

let%expect_test "base64url vectors (RFC 4648)" =
  List.iter
    [ ""; "f"; "fo"; "foo"; "foob"; "fooba"; "foobar"; "\xfb\xff\xbf" ]
    ~f:(fun s ->
      let e = Codec.base64url_encode s in
      printf
        "%S -> %s -> %b\n"
        s
        e
        ([%equal: string option] (Codec.base64url_decode e) (Some s)));
  [%expect
    {|
    "" ->  -> true
    "f" -> Zg -> true
    "fo" -> Zm8 -> true
    "foo" -> Zm9v -> true
    "foob" -> Zm9vYg -> true
    "fooba" -> Zm9vYmE -> true
    "foobar" -> Zm9vYmFy -> true
    "\251\255\191" -> -_-_ -> true
    |}]
;;

let%expect_test "base64url rejects the standard alphabet and padding" =
  List.iter [ "+w"; "/w"; "Zg=="; "Z"; "Zm9v!" ] ~f:(fun s ->
    print_s [%sexp (s : string), (Codec.base64url_decode s : string option)]);
  [%expect {|
    (+w ())
    (/w ())
    (Zg== ())
    (Z ())
    (Zm9v! ())
    |}]
;;

let%test_unit "base64url round trip" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:String.sexp_of_t ~f:(fun s ->
    [%test_result: string option]
      (Codec.base64url_decode (Codec.base64url_encode s))
      ~expect:(Some s))
;;

let%expect_test "hmac_sha256 (RFC 4231 test case 2)" =
  print_endline (Codec.hex (Codec.hmac_sha256 ~key:"Jefe" "what do ya want for nothing?"));
  print_endline (Codec.sha256_hex "abc");
  [%expect
    {|
    5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843
    ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
    |}]
;;

let%expect_test "equal_constant_time" =
  print_s
    [%sexp
      (Codec.equal_constant_time "abc" "abc" : bool)
      , (Codec.equal_constant_time "abc" "abd" : bool)
      , (Codec.equal_constant_time "abc" "ab" : bool)];
  print_s [%sexp (String.length (Codec.random_bytes 16) : int)];
  [%expect {|
    (true false false)
    16
    |}]
;;
