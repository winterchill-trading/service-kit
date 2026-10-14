(** Token-bucket rate limits per key, with the time passed in.

    A bucket holds up to [burst] tokens and gains one every [refill_every]; each request
    takes [cost] tokens (default 1) or is refused with the time until enough tokens are
    back. The state of a bucket is a single timestamp (the generic cell rate algorithm),
    so the arithmetic is exact in integer nanoseconds and the functions are pure: the
    caller supplies [now], usually from a {!Clock.t}.

    {[
      let limits = Rate_limit.create (Rate_limit.Config.per_minute ~burst:20 120) in
      match Rate_limit.check limits ~key:client_ip ~now:(Clock.now clock) with
      | Allowed _ -> handle request
      | Denied { retry_after } -> Problem.too_many_requests ~retry_after
    ]} *)

open! Core

module Config : sig
  type t = private
    { burst : int (** bucket size, at least 1 *)
    ; refill_every : Time_ns.Span.t (** time to gain one token, positive *)
    }
  [@@deriving sexp, compare, equal]

  (** Raises unless [burst >= 1] and [refill_every] is positive. *)
  val create : burst:int -> refill_every:Time_ns.Span.t -> t

  (** [n] tokens a minute (at least 1), [burst] at once. *)
  val per_minute : burst:int -> int -> t
end

module Decision : sig
  type t =
    | Allowed of { remaining : int (** whole tokens left after this request *) }
    | Denied of { retry_after : Time_ns.Span.t (** wait at least this long *) }
  [@@deriving sexp, compare, equal]
end

(** One bucket. *)
module Bucket : sig
  type t [@@deriving sexp, compare, equal]

  (** A full bucket. *)
  val full : t

  (** Takes [cost] tokens (default 1) at [now]. On [Denied] the bucket is unchanged. A
      [cost] above [burst] is always denied, with [retry_after] the time to refill a whole
      bucket. *)
  val take : ?cost:int -> Config.t -> t -> now:Time_ns.t -> Decision.t * t

  (** Whole tokens in the bucket at [now]. *)
  val available : Config.t -> t -> now:Time_ns.t -> int
end

(** Buckets keyed by a string (client address, user id, route and user, ...). *)
type t

(** [max_keys] (default 10_000) bounds memory: past it, buckets that have refilled
    completely are dropped, which loses nothing since a missing key reads as full. *)
val create : ?max_keys:int -> Config.t -> t

(** {!Bucket.take} on [key]'s bucket. *)
val check : ?cost:int -> t -> key:string -> now:Time_ns.t -> Decision.t

(** Number of keys with a bucket in memory. *)
val tracked : t -> int

(** The configuration [t] was created with. *)
val config : t -> Config.t

(** Refuses requests over the limit with 429 [rate_limited] and [Retry-After]. [key] picks
    the bucket (default: the peer address, or ["unknown"]); [cost] the tokens a request
    takes (default 1). Use one limiter per policy, e.g. a strict one as route middleware
    on sign-in and a loose one globally. *)
val middleware
  :  ?key:(Request.t -> string)
  -> ?cost:(Request.t -> int)
  -> t
  -> clock:Clock.t
  -> Router.Middleware.t
