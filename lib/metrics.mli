(** Counters, gauges and histograms in the Prometheus text exposition format (version
    0.0.4), for a [GET /metrics] route.

    Metrics belong to a registry and may have labels. Label values come from code, not
    from requests: label a request by its route pattern ([/accounts/:id]), never by its
    raw path, or a client can grow the registry without bound.

    {[
      let registry = Metrics.create () in
      let logins =
        Metrics.Counter.create
          registry
          "logins_total"
          ~help:"Sign-ins"
          ~labels:[ "result" ]
      in
      Metrics.Counter.incr logins [ "ok" ];
      print_string (Metrics.render registry)
    ]}

    prints

    {v
# HELP logins_total Sign-ins
# TYPE logins_total counter
logins_total{result="ok"} 1
    v} *)

open! Core

type t

(** An empty registry. [namespace], when given, is prefixed to every name with an
    underscore. *)
val create : ?namespace:string -> unit -> t

module Counter : sig
  type registry := t
  type t

  (** Registers a counter. Raises on an invalid metric or label name, or a name already
      registered. *)
  val create : ?help:string -> ?labels:string list -> registry -> string -> t

  (** Adds [by] (default 1) to the series with these label values. Raises when [by] is
      negative or the number of values differs from the number of labels. *)
  val incr : ?by:float -> t -> string list -> unit
end

module Gauge : sig
  type registry := t
  type t

  (** Registers a gauge; raises like {!Counter.create}. *)
  val create : ?help:string -> ?labels:string list -> registry -> string -> t

  (** Sets the series with these label values. *)
  val set : t -> string list -> float -> unit

  (** Adds [delta] (may be negative). *)
  val add : t -> string list -> float -> unit

  (** Registers a gauge without labels whose value [f] computes at each render, e.g. a
      connection count or a queue length. *)
  val computed : ?help:string -> registry -> string -> (unit -> float) -> unit
end

module Histogram : sig
  type registry := t
  type t

  (** The Prometheus client default: 5 ms to 10 s. *)
  val default_buckets : float list

  (** Registers a histogram with these upper bounds (sorted, deduplicated; [+Inf] is
      implicit). Raises like {!Counter.create}, or when [le] is used as a label. *)
  val create
    :  ?help:string
    -> ?labels:string list
    -> ?buckets:float list
    -> registry
    -> string
    -> t

  (** Records one observation. *)
  val observe : t -> string list -> float -> unit
end

(** The standard per-request series: [http_requests_total{route,method,status}] and
    [http_request_duration_seconds{route,method}]. *)
module Requests : sig
  type registry := t
  type t

  (** Registers both series in [registry]. *)
  val create : registry -> t

  (** Counts one request. [route] is the route pattern. *)
  val observe
    :  t
    -> route:string
    -> meth:string
    -> status:int
    -> duration:Time_ns.Span.t
    -> unit

  (** Observes every request that passes through, labelled with {!Request.route}. *)
  val middleware : t -> clock:Clock.t -> Router.Middleware.t
end

(** [GET /metrics] serving {!render}. *)
val route : t -> Router.Route.t

(** The media type of {!render}'s output. *)
val content_type : string

(** Every metric in the text format, sorted by name, then by label values. *)
val render : t -> string
