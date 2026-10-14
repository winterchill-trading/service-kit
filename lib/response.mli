(** A response before it is written: status, headers and a string body. *)

open! Core

type t =
  { status : Cohttp.Code.status_code
  ; headers : (string * string) list (** lower-case names by convention *)
  ; body : string
  }
[@@deriving sexp_of]

(** A JSON body with [content-type: application/json] (status 200 by default). *)
val json
  :  ?status:Cohttp.Code.status_code
  -> ?headers:(string * string) list
  -> Yojson.Safe.t
  -> t

(** A text body ([text/plain; charset=utf-8] by default). *)
val text
  :  ?status:Cohttp.Code.status_code
  -> ?headers:(string * string) list
  -> ?content_type:string
  -> string
  -> t

(** No body. *)
val empty : ?headers:(string * string) list -> Cohttp.Code.status_code -> t

(** The problem document with [application/problem+json] and the problem's own headers.
    See {!Problem.to_yojson} for [type_base]. *)
val problem : ?type_base:string -> Problem.t -> t

(** The status as an integer. *)
val status_code : t -> int

(** The first value of a header, case-insensitively. *)
val header : t -> string -> string option

(** [t] with a header added, replacing any header of the same name. *)
val with_header : t -> string -> string -> t

(** [render_problem ~render req p] is [render req p], plus every header of [p] (such as
    [Allow] or [Retry-After]) the rendered response does not set itself. Modules that
    refuse requests on their own ({!Router}, {!Idempotency}) take such a [render] so
    refusals can use a service's own error body instead of a problem document. *)
val render_problem : render:(Request.t -> Problem.t -> t) -> Request.t -> Problem.t -> t
