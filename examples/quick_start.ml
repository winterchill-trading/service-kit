(* The README's quick start, built in CI so it keeps compiling.

   dune exec examples/quick_start.exe, then curl localhost:8080/hello/world *)

open! Core
open! Async
open Service_kit

let hello name (_ : Request.t) =
  if String.length name > 64
  then
    return
      (Response.problem (Problem.bad_request "name_too_long" ~params:[ "max", Int 64 ]))
  else
    return
      (Response.json
         (Message.to_yojson (Message.create "greeting" ~params:[ "name", String name ])))
;;

let main () =
  let registry = Metrics.create () in
  let requests = Metrics.Requests.create registry in
  let log entry =
    print_endline (Yojson.Safe.to_string (Request_log.Entry.to_yojson entry))
  in
  let router =
    Router.create
      ~middleware:
        [ Request_log.middleware ~clock:Clock.wall ~sink:log ()
        ; Metrics.Requests.middleware requests ~clock:Clock.wall
        ]
      (Router.Route.get Router.Path.(root / "hello" /: string "name") hello
       :: Metrics.route registry
       :: Health.routes (Health.create []))
  in
  let%bind server = Server.start ~port:8080 (Router.handle router) in
  printf "listening on %d\n" (Cohttp_async.Server.listening_on server);
  Deferred.never ()
;;

let () =
  Command_unix.run
    (Command.async ~summary:"service-kit quick start" (Command.Param.return main))
;;
