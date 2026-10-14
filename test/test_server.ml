open! Core
open! Async
open Service_kit

let%expect_test "over HTTP" =
  let router =
    Router.create
      Router.
        [ Route.post
            Path.(root / "echo")
            (fun req ->
              return
                (Response.json
                   (`Assoc
                     [ "length", `Int (String.length req.body)
                     ; "local", `Bool (Option.is_some req.remote)
                     ])))
        ; Route.get Path.(root / "boom") (fun _ -> failwith "boom")
        ]
  in
  let%bind server =
    Server.start
      ~max_body:16
      ~on_exn:(fun exn -> print_s [%message "on_exn" (exn : exn)])
      ~port:0
      (Router.handle router)
  in
  let port = Cohttp_async.Server.listening_on server in
  let uri path = Uri.of_string (sprintf "http://127.0.0.1:%d%s" port path) in
  let show (resp, body) =
    let%map body = Cohttp_async.Body.to_string body in
    printf
      "%d %s %s\n"
      (Cohttp.Code.code_of_status (Cohttp.Response.status resp))
      (Option.value
         (Cohttp.Header.get (Cohttp.Response.headers resp) "content-type")
         ~default:"-")
      body
  in
  let%bind () =
    Cohttp_async.Client.post (uri "/echo") ~body:(Cohttp_async.Body.of_string "hello")
    >>= show
  in
  let%bind () =
    Cohttp_async.Client.post
      (uri "/echo")
      ~body:(Cohttp_async.Body.of_string (String.make 17 'x'))
    >>= show
  in
  let%bind () = Cohttp_async.Client.get (uri "/boom") >>= show in
  let%bind () = Cohttp_async.Client.get (uri "/missing") >>= show in
  let%bind () = Cohttp_async.Server.close server in
  [%expect
    {|
    200 application/json {"length":5,"local":true}
    413 application/problem+json {"type":"about:blank","title":"Request Entity Too Large","status":413,"code":"payload_too_large","params":{"max_bytes":16}}
    (on_exn (exn (Failure boom)))
    500 application/problem+json {"type":"about:blank","title":"Internal Server Error","status":500,"code":"internal_error"}
    404 application/problem+json {"type":"about:blank","title":"Not Found","status":404,"code":"not_found"}
    |}];
  return ()
;;
