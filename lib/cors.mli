(** Cross-origin resource sharing as a {!Router.Middleware.t}.

    Preflight requests ([OPTIONS] with [Origin] and [Access-Control-Request-Method]) are
    answered 204 by the middleware without reaching any route: with the CORS headers when
    the origin is allowed, without them otherwise (the browser then blocks the call).
    Other requests from an allowed origin get [Access-Control-Allow-Origin] and the
    exposed headers on their response. Every response that depends on [Origin] carries
    [Vary: Origin] so caches keep them apart. *)

open! Core

(** A CORS policy. *)
type t

(** [origins] lists exact origins such as ["https://app.example.com"]; [`Any] allows every
    origin ([*]). [methods] defaults to GET, POST, PUT, PATCH, DELETE; [allow_headers] to
    [authorization], [content-type], [idempotency-key] and [x-request-id];
    [expose_headers] to [x-request-id], [retry-after] and [idempotent-replayed]; [max_age]
    to 10 minutes. Raises when [credentials] is combined with [`Any], which browsers
    reject. *)
val create
  :  ?methods:Cohttp.Code.meth list
  -> ?allow_headers:string list
  -> ?expose_headers:string list
  -> ?max_age:Time_ns.Span.t
  -> ?credentials:bool
  -> origins:[ `Any | `Only of string list ]
  -> unit
  -> t

(** Whether requests from [origin] are allowed. *)
val allows : t -> string -> bool

(** The middleware. *)
val middleware : t -> Router.Middleware.t
