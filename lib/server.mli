(** Serving a {!Router.handler} with cohttp-async.

    The adapter reads the body (refusing anything larger than [max_body] with 413
    [payload_too_large], by [Content-Length] before reading and by size while reading),
    builds a {!Request.t} with the peer address, runs the handler, and turns an exception
    into 500 [internal_error] after passing it to [on_exn]. *)

open! Core
open! Async

(** The cohttp handler. [max_body] defaults to 1 MiB; [on_exn] to logging the exception on
    [Log.Global]; [type_base] is passed to {!Response.problem}. *)
val to_cohttp
  :  ?max_body:int
  -> ?on_exn:(exn -> unit)
  -> ?type_base:string
  -> Router.handler
  -> body:Cohttp_async.Body.t
  -> Socket.Address.Inet.t
  -> Cohttp.Request.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

(** Listens on [port] (0 picks a free one; see [Cohttp_async.Server.listening_on]). *)
val start
  :  ?max_body:int
  -> ?on_exn:(exn -> unit)
  -> ?type_base:string
  -> port:int
  -> Router.handler
  -> (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t
