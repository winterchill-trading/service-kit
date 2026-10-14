(** RFC 9457 problem details ([application/problem+json]) with stable codes.

    A problem carries an HTTP status, a machine-readable [code] and typed {!Param.t}
    values; it never carries a sentence. Clients look the code up in their own catalogue
    and render it in the user's language. The [title] member required by the RFC is the
    status's reason phrase, so it is fixed text too.

    {[
      Problem.create
        `Conflict
        "insufficient_funds"
        ~params:[ "available", Param.Decimal "12.50"; "currency", String "EUR" ]
    ]}

    renders as

    {v
    {"type":"about:blank","title":"Conflict","status":409,"code":"insufficient_funds",
     "params":{"available":"12.50","currency":"EUR"}}
    v} *)

open! Core

type t =
  { status : Cohttp.Code.status_code
  ; code : string (** {!Param.is_valid_name} *)
  ; params : (string * Param.t) list
  ; instance : string option (** e.g. the request id, echoed as the [instance] member *)
  ; headers : (string * string) list
  (** extra response headers, such as [Retry-After] or [WWW-Authenticate] *)
  }
[@@deriving sexp_of, compare, equal]

(** The media type, ["application/problem+json"]. *)
val content_type : string

(** A problem. Raises if [code] or a parameter name is not a valid name
    ({!Param.is_valid_name}), or a parameter name repeats: codes are written in code, not
    computed, so a bad one is a bug. *)
val create
  :  ?params:(string * Param.t) list
  -> ?instance:string
  -> ?headers:(string * string) list
  -> Cohttp.Code.status_code
  -> string
  -> t

(** 400 with the given code. *)
val bad_request : ?params:(string * Param.t) list -> string -> t

(** 401 with the given code. *)
val unauthorized : ?params:(string * Param.t) list -> string -> t

(** 403 with the given code. *)
val forbidden : ?params:(string * Param.t) list -> string -> t

(** 404 with the given code. *)
val not_found : ?params:(string * Param.t) list -> string -> t

(** 409 with the given code. *)
val conflict : ?params:(string * Param.t) list -> string -> t

(** 422 with the given code. *)
val unprocessable : ?params:(string * Param.t) list -> string -> t

(** 405 [method_not_allowed] with an [Allow] header listing [allowed]. *)
val method_not_allowed : allowed:Cohttp.Code.meth list -> t

(** 413 [payload_too_large] with the limit as [max_bytes]. *)
val payload_too_large : max_bytes:int -> t

(** 429 [rate_limited] with a [Retry-After] header and a [retry_after_seconds] param, both
    the span rounded up to whole seconds (at least 1). *)
val too_many_requests : retry_after:Time_ns.Span.t -> t

(** 500 [internal_error], without params. Log the cause; never put it in the response. *)
val internal : t

(** 503 with the given code. *)
val service_unavailable : ?params:(string * Param.t) list -> string -> t

(** The status as an integer. *)
val status_code : t -> int

(** The reason phrase of the status, used as the RFC's [title]. *)
val title : t -> string

(** [t] with [instance] set. *)
val with_instance : t -> string -> t

(** The problem document. [type] is [type_base ^ code] when [type_base] is given (for
    example ["https://errors.example.com/"]), otherwise ["about:blank"]. *)
val to_yojson : ?type_base:string -> t -> Yojson.Safe.t

(** Reads a problem document back, for clients and tests. JSON does not say whether a
    string param was a decimal or a time, so strings decode as {!Param.String}; ints,
    bools and arrays decode as themselves. Headers are not part of the document and come
    back empty. *)
val of_yojson : Yojson.Safe.t -> t Or_error.t
