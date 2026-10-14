(** An HTTP request with its body read, as handlers and middleware see it. *)

open! Core

type t =
  { meth : Cohttp.Code.meth
  ; uri : Uri.t
  ; path : string list (** percent-decoded segments, empty segments dropped *)
  ; headers : Http.Header.t
  ; body : string
  ; remote : string option (** the peer address, when served over a socket *)
  ; route : string
  (** the matched route pattern (e.g. [/accounts/:id]), set by {!Router.handle};
      ["unmatched"] when no route matched *)
  ; request_id : string (** set by {!Request_log.middleware}; empty until then *)
  ; context : Univ_map.t (** values middleware hands to handlers, see {!set} *)
  }

(** A request; [uri] may be a bare path such as ["/accounts/7?limit=5"]. *)
val create
  :  ?headers:(string * string) list
  -> ?body:string
  -> ?remote:string
  -> Cohttp.Code.meth
  -> string
  -> t

(** Splits and percent-decodes a URI path: ["/a//b%2Fc/"] is [["a"; "b/c"]]. *)
val split_path : string -> string list

(** The first value of a header, case-insensitively. *)
val header : t -> string -> string option

(** The first value of a query parameter. *)
val query : t -> string -> string option

(** The body as JSON; an empty body reads as [{}]. 400 [invalid_json] otherwise. *)
val json : t -> (Yojson.Safe.t, Problem.t) Result.t

(** {!json}, then [of_yojson]; any exception from [of_yojson] becomes 400 [invalid_body]
    (decoder messages can name source files, so they are not returned). *)
val decode : t -> (Yojson.Safe.t -> 'a) -> ('a, Problem.t) Result.t

(** [t] with [key] bound to [value] in its context. *)
val set : t -> 'a Univ_map.Key.t -> 'a -> t

(** The value bound to [key], if a middleware set one. *)
val find : t -> 'a Univ_map.Key.t -> 'a option
