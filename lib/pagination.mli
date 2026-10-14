(** Cursor (keyset) pagination.

    A cursor is the position after the last item of a page, for example the
    [(created_at, id)] of the last row, in bin_prot behind a version byte and URL-safe
    base64. With a [key], 16 bytes of HMAC-SHA256 are appended so clients cannot craft
    positions; either way clients must treat cursors as opaque.

    {[
      module Cursor = Pagination.Cursor (struct
          type t = Time_ns.Stable.V1.t * string [@@deriving bin_io, sexp_of]
        end)

      let list_payments ~cursor ~limit =
        let after = Option.map cursor ~f:(Cursor.decode ~key) in
        (* fetch [limit + 1] rows after [after] in a stable order *)
        Pagination.Page.of_overfetch rows ~limit ~cursor:(fun p ->
          Cursor.encode ~key (p.created_at, p.id))
      ;;
    ]} *)

open! Core

(** Cursors over positions of type [Position.t]. *)
module Cursor (Position : sig
    type t [@@deriving bin_io, sexp_of]
  end) : sig
  (** The cursor for [position]. *)
  val encode : ?key:string -> Position.t -> string

  (** The position in a cursor made by {!encode} with the same [key]. Fails with 400
      [invalid_cursor] on anything else. *)
  val decode : ?key:string -> string -> (Position.t, Problem.t) Result.t
end

(** The page size from a [limit] query parameter: [default] (50) when absent, 400
    [invalid_limit] with [max] as a param when not an integer in [1, max] (max 200 by
    default). *)
val limit : ?default:int -> ?max:int -> string option -> (int, Problem.t) Result.t

(** One page of results. *)
module Page : sig
  type 'a t =
    { items : 'a list
    ; next_cursor : string option (** [None] on the last page *)
    }
  [@@deriving sexp_of, compare, equal]

  (** Given up to [limit + 1] items fetched in order, the first [limit] of them and, when
      there was one more, the cursor of the last item kept. Fetching one extra row tells
      whether another page exists without a count query. *)
  val of_overfetch : 'a list -> limit:int -> cursor:('a -> string) -> 'a t

  (** [{"items": [...], "next_cursor": "..." | null}]. *)
  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end
