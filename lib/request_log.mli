(** One structured log entry per request, with request ids and redaction.

    The middleware takes the caller's [X-Request-Id] when it is a sensible token (1-128
    characters of [A-Za-z0-9._:-]) and makes one up otherwise, stores it in
    {!Request.request_id} for handlers (to pass on to downstream calls) and echoes it in
    the response. When the response is out it hands an {!Entry.t} to the sink.

    Secrets are kept out of entries by redaction: header values and query parameters named
    in {!Redact} are replaced by ["REDACTED"], and a hook can rewrite any other value.
    Headers are only logged when listed in [log_headers]. *)

open! Core

module Entry : sig
  type t =
    { request_id : string
    ; meth : string
    ; route : string (** the route pattern *)
    ; target : string (** path and query, after redaction *)
    ; status : int
    ; duration : Time_ns.Span.t
    ; remote : string option
    ; headers : (string * string) list (** the [log_headers] present, after redaction *)
    ; fields : (string * string) list (** added by handlers with {!add_field} *)
    }
  [@@deriving sexp_of]

  (** A flat JSON object for log pipelines; [duration_ms] is a float. *)
  val to_yojson : t -> Yojson.Safe.t
end

module Redact : sig
  type t

  (** [headers] (case-insensitive) and [query] names are redacted in addition to the
      defaults when [replace_defaults] is false (the default). [hook ~name value] rewrites
      any value that is logged; it runs after the name-based rules. *)
  val create
    :  ?replace_defaults:bool
    -> ?headers:string list
    -> ?query:string list
    -> ?hook:(name:string -> string -> string)
    -> unit
    -> t

  (** Headers [authorization], [cookie], [set-cookie], [proxy-authorization], [x-api-key];
      query parameters [token], [access_token], [refresh_token], [code], [password],
      [secret], [key], [signature]. *)
  val default : t

  (** The value to log for header [name]. *)
  val header : t -> name:string -> string -> string

  (** The URI with redacted query values, as path and query. *)
  val target : t -> Uri.t -> string
end

(** The header carrying the request id, ["x-request-id"]. *)
val header_name : string

(** Whether a caller-supplied id is accepted as is. *)
val valid_request_id : string -> bool

(** Adds a field to this request's entry, e.g. the account a handler acted on. A no-op
    when the request did not pass through {!middleware}. *)
val add_field : Request.t -> string -> string -> unit

(** The middleware. [random] makes request ids (default: 12 secure random bytes, URL-safe
    base64). *)
val middleware
  :  ?redact:Redact.t
  -> ?log_headers:string list
  -> ?random:(unit -> string)
  -> clock:Clock.t
  -> sink:(Entry.t -> unit)
  -> unit
  -> Router.Middleware.t

(** A sink writing each entry as one sexp to an Async log at level [Info]. *)
val log_sink : Async.Log.t -> Entry.t -> unit
