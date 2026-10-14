(** Step-up grants: short-lived proof that the holder of a session re-authenticated
    (passcode, second factor, device confirmation) for one specific action.

    A grant is bound to one session and one purpose, expires after [ttl] (5 minutes by
    default) and can be consumed once. Moving money or revealing a card number would
    {!Make.consume} a grant; read-only checks such as "show the confirm button" would only
    {!Make.check} it.

    Purposes are the application's own variant:

    {[
      module Purpose = struct
        type t =
          | Send_payment
          | Reveal_card
          | Change_passcode
        [@@deriving sexp, compare, equal, enumerate]

        let to_string t = String.lowercase (Sexp.to_string (sexp_of_t t))
        let of_string s = List.find all ~f:(fun p -> String.equal (to_string p) s)
      end

      module Grants = Step_up.Make (Purpose) (Step_up.Memory_spent)
    ]}

    Grants are signed tokens, so checking needs no storage; consuming records the grant id
    in a {!Spent} store until the grant expires, which is the only state. Services running
    several instances give them a shared store.

    {b Retries.} A consumed grant cannot be presented again, so a client whose payment
    request timed out cannot safely retry it with the same grant. In
    {!Mode.bind_on_first_use} mode a grant is instead bound to the first request that uses
    it, identified by an operation name and the request's idempotency key ({!Binding.t});
    the same grant is accepted again only for retries of that request (same operation,
    same key) until it expires, and refused for anything else. Pair it with
    {!Idempotency}, which makes those retries return the original response. *)

open! Core

(** The application's purposes. *)
module type Purpose = sig
  type t [@@deriving sexp, compare, equal]

  (** A stable wire name, written into grants. *)
  val to_string : t -> string

  (** Inverse of [to_string]; [None] for unknown names. *)
  val of_string : string -> t option
end

(** Where consumed grant ids are remembered until the grant would have expired anyway. *)
module type Spent = sig
  type t

  (** Records [grant_id] as used. [`Already_used] if it was recorded before. Must be
      atomic across every instance sharing the store. *)
  val mark
    :  t
    -> grant_id:string
    -> until:Time_ns.t
    -> [ `First_use | `Already_used ] Or_error.t Async.Deferred.t
end

(** An in-process {!Spent} store for one instance and for tests. *)
module Memory_spent : sig
  include Spent

  (** An empty store. Entries are dropped once [until] has passed, checked against [clock]
      on each [mark]. *)
  val create : clock:Clock.t -> unit -> t

  (** Entries currently remembered. *)
  val length : t -> int
end

(** The request a grant is bound to in {!Mode.bind_on_first_use} mode. *)
module Binding : sig
  type t =
    { operation : string
    (** what the request does; {!Make.require} uses the method and route pattern, such as
        ["POST /payments"] *)
    ; idempotency_key : string (** the client's key for this request *)
    }
  [@@deriving sexp, compare, equal]
end

(** Where grant bindings are remembered until the grant would have expired anyway. *)
module type Bindings = sig
  type t

  (** Binds [grant_id] to [binding] if it is unbound ([`Bound]); otherwise reports whether
      the existing binding is the same request ([`Same_request]) or another one
      ([`Other_request], with the existing binding). Must be atomic across every instance
      sharing the store. *)
  val bind
    :  t
    -> grant_id:string
    -> binding:Binding.t
    -> until:Time_ns.t
    -> [ `Bound | `Same_request | `Other_request of Binding.t ] Or_error.t
         Async.Deferred.t
end

(** A {!Bindings} store whose [bind] is told the current time, read from the clock given
    to {!Make.create}. A store over a database compares [now] with stored expiry times
    instead of the database's own clock, so a service and its tests (with a manual clock)
    agree on when a binding has lapsed. *)
module type Timed_bindings = sig
  type t

  (** As {!Bindings.bind}; [now] is the grant checker's current time. *)
  val bind
    :  t
    -> grant_id:string
    -> binding:Binding.t
    -> until:Time_ns.t
    -> now:Time_ns.t
    -> [ `Bound | `Same_request | `Other_request of Binding.t ] Or_error.t
         Async.Deferred.t
end

(** An in-process {!Bindings} store for one instance and for tests. *)
module Memory_bindings : sig
  include Bindings

  (** An empty store. Entries are dropped once [until] has passed, checked against [clock]
      on each [bind]. *)
  val create : clock:Clock.t -> unit -> t

  (** The same store as a {!Timed_bindings}: entries are dropped once [until] has passed,
      checked against the [now] each call is given ([clock] is not read). *)
  module Timed : Timed_bindings with type t = t

  (** Entries currently remembered. *)
  val length : t -> int
end

(** How a grant may be used. *)
module Mode : sig
  type t

  (** A grant is used once; the second use fails with [Already_used]. The default. *)
  val consume_once : t

  (** A grant is bound to the first request that uses it and accepted again only for
      retries of that request; any other request gets [Already_used]. Bindings live in
      [store]; the {!Spent} store is not consulted in this mode. *)
  val bind_on_first_use : (module Bindings with type t = 'a) -> 'a -> t

  (** {!bind_on_first_use} over a {!Timed_bindings} store: every [bind] receives the
      current time from the clock given to {!Make.create}. *)
  val bind_on_first_use_timed : (module Timed_bindings with type t = 'a) -> 'a -> t
end

(** Why a grant was refused. *)
module Error : sig
  type t =
    | Malformed
    | Bad_signature
    | Expired
    | Wrong_session
    | Wrong_purpose
    | Already_used
    | Store_failed of Core.Error.t
  [@@deriving sexp_of]

  (** 500 [internal_error] for [Store_failed] (log the cause), otherwise 403
      [step_up_required]: whatever went wrong, the client's next step is a new step-up. *)
  val to_problem : t -> Problem.t
end

(** Grants for the application's purposes, consumed through [Spent]. *)
module Make (Purpose : Purpose) (Spent : Spent) : sig
  (** A verified grant. *)
  module Grant : sig
    type t =
      { grant_id : string
      ; session_id : string
      ; purpose : Purpose.t
      ; issued_at : Time_ns.t
      ; expires_at : Time_ns.t
      }
    [@@deriving sexp_of, compare, equal]
  end

  type t

  (** [key] signs grants and must be at least 32 bytes (raises otherwise); use a different
      key from the session keyring. [mode] defaults to {!Mode.consume_once}. *)
  val create
    :  ?mode:Mode.t
    -> ?ttl:Time_ns.Span.t
    -> ?random:(int -> string)
    -> key:string
    -> spent:Spent.t
    -> clock:Clock.t
    -> unit
    -> t

  (** A grant for [purpose] in [session_id]; call it after the user re-authenticated. *)
  val issue : t -> session_id:string -> purpose:Purpose.t -> string * Grant.t

  (** Whether the token is a valid, unexpired grant for this session and purpose, without
      using it up. Does not consult the spent store. *)
  val check
    :  t
    -> string
    -> session_id:string
    -> purpose:Purpose.t
    -> (Grant.t, Error.t) Result.t

  (** {!check}, then marks the grant used; a second consume of the same token fails with
      [Already_used]. In {!Mode.bind_on_first_use} mode the grant is bound to a fresh
      request nobody can repeat, so it is used once just the same; use {!use} to allow
      retries. *)
  val consume
    :  t
    -> string
    -> session_id:string
    -> purpose:Purpose.t
    -> (Grant.t, Error.t) Result.t Async.Deferred.t

  (** {!check}, then uses the grant for the request [binding]. In
      {!Mode.bind_on_first_use} mode the first use binds the grant to [binding], later
      uses with an equal binding succeed and any other binding fails with [Already_used].
      In {!Mode.consume_once} mode this is {!consume} and [binding] is ignored. *)
  val use
    :  t
    -> string
    -> session_id:string
    -> purpose:Purpose.t
    -> binding:Binding.t
    -> (Grant.t, Error.t) Result.t Async.Deferred.t

  (** The request header carrying a grant, ["x-step-up-grant"]. *)
  val header_name : string

  (** Route middleware that consumes the grant in {!header_name} for [purpose] in the
      session [session_id] names (typically from {!Session.claims}). Without a session, a
      grant, or with any grant error, answers 403 [step_up_required] with the purpose as
      the [purpose] param, so the client knows which step-up to start.

      In {!Mode.bind_on_first_use} mode the grant is used through {!use} with the
      request's [operation] (default: method and route pattern, such as
      ["POST /payments"]) and its {!Idempotency.header_name} header; a request without
      that header can only use the grant once. *)
  val require
    :  ?operation:(Request.t -> string)
    -> t
    -> purpose:Purpose.t
    -> session_id:(Request.t -> string option)
    -> Router.Middleware.t
end

(** The signed payload. Purposes are stored by their wire name. *)
module Stable : sig
  module Claims : sig
    module V1 : sig
      type t =
        { grant_id : string
        ; session_id : string
        ; purpose : string
        ; issued_at : Time_ns.Stable.V1.t
        ; expires_at : Time_ns.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end
end
