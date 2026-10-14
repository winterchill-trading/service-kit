(** Structured server messages: a stable code and typed parameters, rendered by the client
    in the user's language.

    A server that sends ["Payment of 12.50 EUR sent"] has decided the language, the number
    format and the wording for every client. A server that sends

    {v {"code":"payment_sent","params":{"amount":"12.50","currency":"EUR"}} v}

    leaves those to the client and keeps the meaning testable. A {!Catalogue} declares,
    for every code, the parameters it takes and a template per locale; tests check that
    every code the service can emit has an entry and every entry covers every locale. *)

open! Core

type t =
  { code : string
  ; params : (string * Param.t) list
  }
[@@deriving sexp, compare, equal]

(** A message. Raises on an invalid code or parameter name ({!Param.is_valid_name}) or a
    repeated parameter. *)
val create : ?params:(string * Param.t) list -> string -> t

(** [{"code": ..., "params": {...}}]; [params] is omitted when empty. *)
val to_yojson : t -> Yojson.Safe.t

(** What a service knows about its codes. *)
module Catalogue : sig
  module Entry : sig
    type t =
      { params : (string * Param.Kind.t) list (** every parameter the code takes *)
      ; templates : (string * string) list
      (** locale (e.g. ["en"], ["pt-BR"]) to template; [{name}] stands for a parameter and
          [{{] / [}}] for literal braces *)
      }
    [@@deriving sexp, compare, equal]
  end

  type message := t
  type t [@@deriving sexp_of]

  (** A catalogue. Fails on a repeated or invalid code, an invalid or repeated parameter
      name, or a template that names an undeclared parameter or has an unbalanced brace. *)
  val create : (string * Entry.t) list -> t Or_error.t

  (** The entry for [code]. *)
  val find : t -> string -> Entry.t option

  (** Every code, sorted. *)
  val codes : t -> string list

  (** Fails unless [message]'s code has an entry and its parameters are exactly the
      declared ones (any order) with the declared kinds. *)
  val validate : t -> message -> unit Or_error.t

  (** {!validate} for a problem's code and parameters. *)
  val validate_problem : t -> Problem.t -> unit Or_error.t

  (** Fails with every gap at once: a code in [codes] without an entry, an entry without a
      template for one of [locales], and (when [exhaustive], the default) an entry whose
      code is not in [codes]. Run it in a test over the full list of codes the service
      emits. *)
  val check
    :  ?exhaustive:bool
    -> t
    -> codes:string list
    -> locales:string list
    -> unit Or_error.t

  (** Fills in the template for [locale], or [fallback] when [locale] has none. Decimals
      and strings render as given, ints and bools in plain ASCII, times as RFC 3339 UTC,
      lists comma-separated. Meant for server-rendered text such as e-mails and logs;
      clients render from the code themselves. *)
  val render : ?fallback:string -> t -> locale:string -> message -> string Or_error.t

  (** The catalogue as JSON, for shipping to clients:
      [{"<code>": {"params": {"<name>": "<kind>"}, "templates": {"<locale>": "..."}}}]. *)
  val to_yojson : t -> Yojson.Safe.t
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
