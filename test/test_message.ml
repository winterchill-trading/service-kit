open! Core
open Service_kit

let catalogue =
  Message.Catalogue.create
    [ ( "payment_sent"
      , { params = [ "amount", Decimal; "currency", String ]
        ; templates =
            [ "en", "Payment of {amount} {currency} sent"
            ; "de", "Zahlung von {amount} {currency} gesendet"
            ]
        } )
    ; ( "insufficient_funds"
      , { params = [ "available", Decimal ]
        ; templates = [ "en", "Only {available} available {{per account}}" ]
        } )
    ; ( "signed_out"
      , { params = []; templates = [ "en", "Signed out"; "de", "Abgemeldet" ] } )
    ]
  |> ok_exn
;;

let sent =
  Message.create
    "payment_sent"
    ~params:[ "amount", Decimal "12.50"; "currency", String "EUR" ]
;;

let%expect_test "json" =
  print_endline (Yojson.Safe.to_string (Message.to_yojson sent));
  print_endline (Yojson.Safe.to_string (Message.to_yojson (Message.create "signed_out")));
  [%expect
    {|
    {"code":"payment_sent","params":{"amount":"12.50","currency":"EUR"}}
    {"code":"signed_out"}
    |}]
;;

let%expect_test "validate" =
  let show m =
    print_s [%sexp (Message.Catalogue.validate catalogue m : unit Or_error.t)]
  in
  show sent;
  show (Message.create "payment_sent" ~params:[ "amount", Decimal "1" ]);
  show
    (Message.create "payment_sent" ~params:[ "amount", Int 1; "currency", String "EUR" ]);
  show (Message.create "nope");
  print_s
    [%sexp
      (Message.Catalogue.validate_problem
         catalogue
         (Problem.conflict "insufficient_funds" ~params:[ "available", Decimal "3.00" ])
       : unit Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Error
     ("params do not match the catalogue" (code payment_sent)
      (missing (currency)) (extra ())))
    (Error
     ("param has the wrong kind" (code payment_sent) (name amount)
      (expected decimal)))
    (Error ("unknown code" (code nope)))
    (Ok ())
    |}]
;;

let%expect_test "render" =
  let show locale m =
    print_s
      [%sexp
        (Message.Catalogue.render ~fallback:"en" catalogue ~locale m : string Or_error.t)]
  in
  show "de" sent;
  show "fr" sent;
  show "de" (Message.create "insufficient_funds" ~params:[ "available", Decimal "3.00" ]);
  [%expect
    {|
    (Ok "Zahlung von 12.50 EUR gesendet")
    (Ok "Payment of 12.50 EUR sent")
    (Ok "Only 3.00 available {per account}")
    |}]
;;

let%expect_test "check reports every gap" =
  print_s
    [%sexp
      (Message.Catalogue.check
         catalogue
         ~codes:[ "payment_sent"; "signed_out"; "card_frozen" ]
         ~locales:[ "en"; "de" ]
       : unit Or_error.t)];
  print_s
    [%sexp
      (Message.Catalogue.check
         ~exhaustive:false
         catalogue
         ~codes:[ "signed_out" ]
         ~locales:[ "en" ]
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     (Message.Catalogue.check
      (("no entry" (code card_frozen))
       ("no template" (code insufficient_funds) (locale de))
       ("unused entry" (code insufficient_funds)))))
    (Ok ())
    |}]
;;

let%expect_test "bad catalogues" =
  let show entries =
    print_s
      [%sexp
        (Message.Catalogue.create entries |> Or_error.map ~f:(fun _ -> ())
         : unit Or_error.t)]
  in
  show [ "a", { params = []; templates = [ "en", "Hi {name}" ] } ];
  show [ "a", { params = [ "name", String ]; templates = [ "en", "Hi {name" ] } ];
  show [ "a", { params = []; templates = [ "en", "a } b" ] } ];
  show [ "a", { params = []; templates = [] }; "a", { params = []; templates = [] } ];
  show [ "A", { params = []; templates = [] } ];
  [%expect
    {|
    (Error
     ("Message.Catalogue: template names an undeclared param" (code a)
      (locale en) (name name)))
    (Error
     ((Message.Catalogue (code a) (locale en))
      ("unclosed brace" (template "Hi {name"))))
    (Error
     ((Message.Catalogue (code a) (locale en))
      ("unbalanced closing brace" (template "a } b"))))
    (Error ("Message.Catalogue: repeated code" (code a)))
    (Error ("Message.Catalogue: invalid code" (code A)))
    |}]
;;

let%expect_test "catalogue json" =
  print_endline (Yojson.Safe.pretty_to_string (Message.Catalogue.to_yojson catalogue));
  [%expect
    {|
    {
      "insufficient_funds": {
        "params": { "available": "decimal" },
        "templates": { "en": "Only {available} available {{per account}}" }
      },
      "payment_sent": {
        "params": { "amount": "decimal", "currency": "string" },
        "templates": {
          "en": "Payment of {amount} {currency} sent",
          "de": "Zahlung von {amount} {currency} gesendet"
        }
      },
      "signed_out": {
        "params": {},
        "templates": { "en": "Signed out", "de": "Abgemeldet" }
      }
    }
    |}]
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Message.Stable.V1.t];
  [%expect {| 7a6ba9ac6e113e6d1d3504fec3736588 |}]
;;

let%test_unit "bin_io round trip" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind code = Gens.name
    and values = List.gen_with_length 3 Gens.param_of_kind in
    return
      (Message.create code ~params:(List.mapi values ~f:(fun i v -> sprintf "p%d" i, v)))
  in
  Quickcheck.test gen ~sexp_of:Message.sexp_of_t ~f:(fun m ->
    let buf = Binable.to_string (module Message.Stable.V1) m in
    [%test_result: Message.t] (Binable.of_string (module Message.Stable.V1) buf) ~expect:m)
;;
