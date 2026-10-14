open! Core
open! Async
open Service_kit

let reply text (_ : Request.t) = return (Response.text text)

let router ?middleware () =
  Router.create
    ?middleware
    Router.
      [ Route.get Path.(root) (reply "home")
      ; Route.get Path.(root / "accounts") (reply "accounts")
      ; Route.get
          Path.(root / "accounts" /: string "id")
          (fun id -> reply ("account " ^ id))
      ; Route.post
          Path.(root / "accounts" /: string "id" / "cards" /: int "n")
          (fun id n -> reply (sprintf "card %d of %s" n id))
      ; Route.delete
          Path.(root / "accounts" /: string "id")
          ~middleware:
            [ (fun h req ->
                if Option.is_some (Request.header req "x-admin")
                then h req
                else return (Response.problem (Problem.forbidden "admin_only")))
            ]
          (fun id -> reply ("closed " ^ id))
      ]
;;

let call ?(headers = []) router meth path =
  let%map r = Router.handle router (Request.create ~headers meth path) in
  printf "%d %s\n" (Response.status_code r) r.body;
  Option.iter (Response.header r "allow") ~f:(printf "allow: %s\n")
;;

let%expect_test "dispatch" =
  let r = router () in
  let%bind () = call r `GET "/" in
  let%bind () = call r `GET "/accounts/" in
  let%bind () = call r `GET "/accounts/a%20b" in
  let%bind () = call r `POST "/accounts/a1/cards/7" in
  let%bind () = call r `POST "/accounts/a1/cards/x" in
  let%bind () = call r `PUT "/accounts/a1" in
  let%bind () = call r `DELETE "/accounts/a1" in
  let%bind () = call r `DELETE "/accounts/a1" ~headers:[ "x-admin", "1" ] in
  [%expect
    {|
    200 home
    200 accounts
    200 account a b
    200 card 7 of a1
    404 {"type":"about:blank","title":"Not Found","status":404,"code":"not_found"}
    405 {"type":"about:blank","title":"Method Not Allowed","status":405,"code":"method_not_allowed"}
    allow: GET, DELETE
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"admin_only"}
    200 closed a1
    |}];
  return ()
;;

let%expect_test "global middleware sees the route pattern" =
  let seen = ref [] in
  let log h (req : Request.t) =
    let%map resp = h req in
    seen
    := sprintf
         "%s %s %d"
         (Cohttp.Code.string_of_method req.meth)
         req.route
         (Response.status_code resp)
       :: !seen;
    resp
  in
  let r = router ~middleware:[ log ] () in
  let%bind () = call r `GET "/accounts/42" in
  let%bind () = call r `GET "/nowhere" in
  let%bind () = call r `PATCH "/accounts" in
  List.iter (List.rev !seen) ~f:print_endline;
  print_s [%sexp (Router.routes r : (string * string) list)];
  [%expect
    {|
    200 account 42
    404 {"type":"about:blank","title":"Not Found","status":404,"code":"not_found"}
    405 {"type":"about:blank","title":"Method Not Allowed","status":405,"code":"method_not_allowed"}
    allow: GET
    GET /accounts/:id 200
    GET unmatched 404
    PATCH /accounts 405
    ((GET /) (GET /accounts) (GET /accounts/:id) (POST /accounts/:id/cards/:n)
     (DELETE /accounts/:id))
    |}];
  return ()
;;

let%expect_test "middleware order" =
  let tag name h req =
    printf "> %s\n" name;
    let%map resp = h req in
    printf "< %s\n" name;
    resp
  in
  let h = Router.Middleware.compose [ tag "a"; tag "b" ] (reply "x") in
  let%bind (_ : Response.t) = h (Request.create `GET "/") in
  [%expect {|
    > a
    > b
    < b
    < a
    |}];
  return ()
;;

let%expect_test "repeated routes are refused" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Router.create
      Router.
        [ Route.get Path.(root / "a") (reply "1")
        ; Route.get Path.(root / "a") (reply "2")
        ]);
  [%expect {| ("Router.create: repeated route" GET /a) |}];
  return ()
;;

let%test_unit "parameters come back from encoded paths" =
  let path = Router.Path.(root / "a" /: string "s" /: int "n") in
  Quickcheck.test
    [%quickcheck.generator: string * int]
    ~sexp_of:[%sexp_of: string * int]
    ~f:(fun (s, n) ->
      let encoded = String.concat_map s ~f:(fun c -> sprintf "%%%02X" (Char.to_int c)) in
      let raw = "/a/" ^ encoded ^ "/" ^ Int.to_string n in
      let segments = Request.split_path raw in
      let parsed = Router.Path.parse path segments (fun s n -> s, n) in
      (* An empty segment is dropped, so the path no longer has three segments. *)
      if String.is_empty s
      then assert (Option.is_none parsed)
      else [%test_result: (string * int) option] parsed ~expect:(Some (s, n)))
;;
