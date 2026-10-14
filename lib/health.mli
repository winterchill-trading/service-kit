(** Liveness and readiness endpoints.

    [GET /healthz] answers 200 whenever the process can serve HTTP; orchestrators restart
    the process when it stops answering. [GET /readyz] runs the dependency checks
    (database ping, upstream reachability, ...) concurrently, each under a timeout, and
    answers 503 when a critical one fails, so load balancers stop sending traffic without
    restarting anything. A failing non-critical check reports ["degraded"] with 200.

    {v
    {"status":"unavailable","checks":{"database":"ok","ledger":"failed","rates":"timeout"}}
    v}

    Failure details are left out of responses unless [verbose] is set: they can name hosts
    and users. *)

open! Core
open! Async

module Check : sig
  type t

  (** A check. [critical] defaults to [true], [timeout] to 2 seconds. *)
  val create
    :  ?critical:bool
    -> ?timeout:Time_ns.Span.t
    -> string
    -> (unit -> unit Or_error.t Deferred.t)
    -> t
end

type t

(** A set of checks. *)
val create : ?verbose:bool -> Check.t list -> t

(** While draining, [/readyz] answers 503 with status ["draining"] without running the
    checks; set it at the start of a graceful shutdown. *)
val set_draining : t -> bool -> unit

(** The [/healthz] response. *)
val live : t -> Response.t

(** Runs the checks and builds the [/readyz] response. *)
val ready : t -> Response.t Deferred.t

(** [GET /healthz] and [GET /readyz]. *)
val routes : t -> Router.Route.t list
