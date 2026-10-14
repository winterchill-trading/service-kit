(** Opaque, HMAC-signed session tokens.

    A token is [v1.<key id>.<claims>.<mac>]: the claims are bin_prot in URL-safe base64,
    and the MAC is HMAC-SHA256 over everything before it under the key named by the key
    id. Clients must treat tokens as opaque strings; the claims are signed, not encrypted,
    so they hold ids and times only, never personal data.

    - {b Key rotation}: a {!Keyring} signs with its current key and still accepts tokens
      signed with the previous ones, so a key can be replaced without logging everyone
      out; drop the old key once the longest token lifetime has passed.
    - {b Token rotation}: {!rotate} swaps a valid token for a fresh one in the same
      session, up to a maximum session lifetime.
    - {b Revocation}: the server keeps no session table; [is_revoked] is the hook into
      whatever list the service keeps (by session id for "log out everywhere", by token id
      for a single token). *)

open! Core

module Keyring : sig
  type t

  (** [current] and each of [previous] is [(key id, secret)]. Raises when a key id is not
      1-32 characters of [A-Za-z0-9_-], a key id repeats, or a secret is shorter than 32
      bytes. *)
  val create : ?previous:(string * string) list -> current:string * string -> unit -> t

  (** The id of the key new tokens are signed with. *)
  val current_id : t -> string
end

module Claims : sig
  type t =
    { session_id : string (** 32 hex characters, the same across rotations *)
    ; token_id : string (** 32 hex characters, new on every issue and rotation *)
    ; subject : string (** the application's opaque user or account id *)
    ; started_at : Time_ns.t (** when the session was first issued *)
    ; issued_at : Time_ns.t
    ; expires_at : Time_ns.t
    }
  [@@deriving sexp, compare, equal]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

module Error : sig
  type t =
    | Malformed
    | Unknown_key
    | Bad_signature
    | Expired
    | Revoked
  [@@deriving sexp, compare, equal]

  (** 401 [session_expired] for [Expired], 401 [session_invalid] otherwise: a client must
      not learn whether a forged token had a valid shape, and a revoked session looks like
      any other invalid one. *)
  val to_problem : t -> Problem.t
end

type t

(** [ttl] (default 24 hours) is the lifetime of each token; [max_lifetime] (default 30
    days) caps how long {!rotate} can extend a session. [is_revoked] (default: nothing is
    revoked) is asked after the signature and expiry checks pass. [random] (default: the
    system's secure source) produces the ids; tests pass a deterministic one. *)
val create
  :  ?ttl:Time_ns.Span.t
  -> ?max_lifetime:Time_ns.Span.t
  -> ?is_revoked:(Claims.t -> bool Async.Deferred.t)
  -> ?random:(int -> string)
  -> keyring:Keyring.t
  -> clock:Clock.t
  -> unit
  -> t

(** A new session for [subject]: the token and its claims. *)
val issue : t -> subject:string -> string * Claims.t

(** The claims of a valid token. Checks, in order: shape, key id, MAC (in constant time),
    expiry, then [is_revoked]. *)
val verify : t -> string -> (Claims.t, Error.t) Result.t Async.Deferred.t

(** A fresh token in the same session (same [session_id] and [started_at], new [token_id],
    new expiry no later than [started_at + max_lifetime]), signed with the current key.
    The old token stays valid until it expires unless the caller revokes its [token_id].
    Fails with [Expired] once the session has reached its maximum lifetime. *)
val rotate : t -> string -> (string * Claims.t, Error.t) Result.t Async.Deferred.t

(** The token in an [Authorization: Bearer <token>] header value (scheme matched
    case-insensitively). *)
val of_authorization_header : string -> string option

(** Authenticates requests by their [Authorization: Bearer] token and makes the claims
    available to handlers through {!claims}. A missing [Authorization] header is answered
    401 [session_required] and an invalid token by {!Error.to_problem}, both with
    [WWW-Authenticate: Bearer]; with [optional], a request without any [Authorization]
    header passes through without claims (an invalid token is still refused). *)
val middleware : ?optional:bool -> t -> Router.Middleware.t

(** The claims {!middleware} verified for this request. *)
val claims : Request.t -> Claims.t option
