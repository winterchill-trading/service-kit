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
    several instances give them a shared store. *)

open! Core

(** The application's purposes. *)
module type Purpose = sig
  type t [@@deriving sexp, compare, equal]

  (** A stable wire name, written into grants. *)
  val to_string : t -> string

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

module Make (Purpose : Purpose) (Spent : Spent) : sig
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
      key from the session keyring. *)
  val create
    :  ?ttl:Time_ns.Span.t
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
      [Already_used]. *)
  val consume
    :  t
    -> string
    -> session_id:string
    -> purpose:Purpose.t
    -> (Grant.t, Error.t) Result.t Async.Deferred.t

  (** The request header carrying a grant, ["x-step-up-grant"]. *)
  val header_name : string

  (** Route middleware that consumes the grant in {!header_name} for [purpose] in the
      session [session_id] names (typically from {!Session.claims}). Without a session, a
      grant, or with any grant error, answers 403 [step_up_required] with the purpose as
      the [purpose] param, so the client knows which step-up to start. *)
  val require
    :  t
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
