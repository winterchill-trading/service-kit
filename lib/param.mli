(** Typed values carried by {!Problem} and {!Message}.

    A client renders a problem or a message from its code and these values, in its own
    language and number format, so the server never sends sentences. Each value keeps its
    type on the wire through the catalogue ({!Message.Catalogue}) rather than in the JSON
    itself: an amount is a [Decimal] string ["12.50"], never a float. *)

open! Core

type t =
  | String of string
  | Int of int
  | Bool of bool
  | Decimal of string (** exact decimal text, e.g. ["-12.50"]; see {!decimal} *)
  | Time of Time_ns.t (** RFC 3339 in UTC on the wire *)
  | List of t list
[@@deriving sexp, compare, equal]

(** The type of a value, used by catalogues to declare what a code expects. *)
module Kind : sig
  type t =
    | String
    | Int
    | Bool
    | Decimal
    | Time
    | List of t
  [@@deriving sexp, compare, equal]

  (** ["string"], ["int"], ["bool"], ["decimal"], ["time"], ["list(<kind>)"]. *)
  val to_string : t -> string
end

(** [Decimal s] after checking that [s] matches [-?[0-9]+(\.[0-9]+)?]. *)
val decimal : string -> t Or_error.t

(** Whether [t] has kind [kind]; every element of a [List] must match [List kind], and an
    empty list matches any list kind. *)
val has_kind : t -> Kind.t -> bool

(** JSON encoding: strings, ints and bools as themselves, decimals as strings, times as
    RFC 3339 strings with nanoseconds and a [Z] suffix, lists as arrays. *)
val to_yojson : t -> Yojson.Safe.t

(** Decodes a value of the given kind (JSON alone cannot tell a decimal or a time from a
    string). *)
val of_yojson : Kind.t -> Yojson.Safe.t -> t Or_error.t

(** Encodes named values as a JSON object, keeping the given order. *)
val params_to_yojson : (string * t) list -> Yojson.Safe.t

(** Whether [name] is a valid parameter or code name: 1 to 64 characters of lower-case
    ASCII letters, digits and underscores, starting with a letter. *)
val is_valid_name : string -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end

  module Kind : sig
    module V1 : sig
      type t = Kind.t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end
