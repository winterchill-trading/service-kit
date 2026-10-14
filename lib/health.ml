open! Core
open! Async

module Check = struct
  type t =
    { name : string
    ; critical : bool
    ; timeout : Time_ns.Span.t
    ; run : unit -> unit Or_error.t Deferred.t
    }

  let create ?(critical = true) ?(timeout = Time_ns.Span.of_sec 2.) name run =
    { name; critical; timeout; run }
  ;;
end

type t =
  { checks : Check.t list
  ; verbose : bool
  ; mutable draining : bool
  }

let create ?(verbose = false) checks = { checks; verbose; draining = false }
let set_draining t b = t.draining <- b
let live (_ : t) = Response.json (`Assoc [ "status", `String "ok" ])

type outcome =
  | Ok_
  | Failed of Error.t
  | Timed_out

let run_check (c : Check.t) =
  match%map
    Clock_ns.with_timeout
      c.timeout
      (Monitor.try_with_or_error ~rest:`Log (fun () -> c.run ()) >>| Or_error.join)
  with
  | `Result (Ok ()) -> Ok_
  | `Result (Error e) -> Failed e
  | `Timeout -> Timed_out
;;

let ready t =
  if t.draining
  then
    return
      (Response.json
         ~status:`Service_unavailable
         (`Assoc [ "status", `String "draining" ]))
  else (
    let%map outcomes =
      Deferred.List.map ~how:`Parallel t.checks ~f:(fun c ->
        let%map o = run_check c in
        c, o)
    in
    let failed critical =
      List.exists outcomes ~f:(fun ((c : Check.t), o) ->
        Bool.equal c.critical critical
        &&
        match o with
        | Ok_ -> false
        | Failed _ | Timed_out -> true)
    in
    let status, word =
      if failed true
      then `Service_unavailable, "unavailable"
      else if failed false
      then `OK, "degraded"
      else `OK, "ok"
    in
    let report ((c : Check.t), o) =
      ( c.name
      , match o with
        | Ok_ -> `String "ok"
        | Timed_out -> `String "timeout"
        | Failed e when t.verbose -> `String ("failed: " ^ Error.to_string_hum e)
        | Failed _ -> `String "failed" )
    in
    Response.json
      ~status
      (`Assoc [ "status", `String word; "checks", `Assoc (List.map outcomes ~f:report) ]))
;;

let routes t =
  Router.
    [ Route.get Path.(root / "healthz") (fun _ -> return (live t))
    ; Route.get Path.(root / "readyz") (fun _ -> ready t)
    ]
;;
