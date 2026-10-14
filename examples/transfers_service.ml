(* Runs the transfers example:

   dune exec examples/transfers_service.exe -- -port 8080

   then, for example:

   TOKEN=$(curl -s -XPOST localhost:8080/sessions -d '[{"user":"alice"}]' | jq -r .token)
   GRANT=$(curl -s -XPOST localhost:8080/step-up -H "authorization: Bearer $TOKEN" \ -d
   '[{"purpose":"send_transfer","passcode":"0000"}]' | jq -r .grant) curl -s -XPOST
   localhost:8080/transfers -H "authorization: Bearer $TOKEN" \ -H "x-step-up-grant:
   $GRANT" -H "idempotency-key: $(uuidgen)" \ -d '[{"to":"bob","amount":"12.50"}]' *)

open! Core
open! Async

let command =
  Command.async
    ~summary:"Serve the service-kit transfers example"
    (let%map_open.Command port =
       flag "-port" (optional_with_default 8080 int) ~doc:"PORT"
     in
     fun () ->
       Log.Global.set_output [ Log.Output.stderr () ];
       let service =
         Transfers_example.Transfers.create
           ~log:(Service_kit.Request_log.log_sink (Lazy.force Log.Global.log))
           ~clock:Service_kit.Clock.wall
           ()
       in
       let%bind server =
         Service_kit.Server.start
           ~port
           (Service_kit.Router.handle (Transfers_example.Transfers.router service))
       in
       Log.Global.info_s
         [%message "listening" ~port:(Cohttp_async.Server.listening_on server : int)];
       Deferred.never ())
;;

let () = Command_unix.run command
