(** Injectable time.

    Every module in this library that needs the current time takes a [Clock.t] (or a plain
    [~now] argument), so tests can move time by hand instead of sleeping. *)

open! Core

(** A source of the current time. *)
type t

(** The system clock ([Time_ns.now]). *)
val wall : t

(** A clock that asks [f] for the time. *)
val of_fun : (unit -> Time_ns.t) -> t

(** A clock that follows an Async time source, so code under
    [Async.Time_source.create ~now ()] sees the synthetic time. *)
val of_time_source : Async.Time_source.t -> t

(** The current time according to [t]. *)
val now : t -> Time_ns.t

(** A clock that only moves when told to. *)
module Manual : sig
  type clock := t
  type t [@@deriving sexp_of]

  (** A manual clock reading [start]. *)
  val create : start:Time_ns.t -> t

  (** The [Clock.t] view of this manual clock. *)
  val clock : t -> clock

  (** The time the clock reads. *)
  val now : t -> Time_ns.t

  (** Moves the clock forward by [span]; raises if [span] is negative. *)
  val advance : t -> Time_ns.Span.t -> unit

  (** Sets the clock to [time]; raises if [time] is earlier than the current reading. *)
  val set : t -> Time_ns.t -> unit
end
