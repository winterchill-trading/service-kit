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

(** Signing keys by id. *)
module Keyring : sig
  type t

  (** [current] and each of [previous] is [(key id, secret)]. Raises when a key id is not
      1-32 characters of [A-Za-z0-9_-], a key id repeats, or a secret is shorter than 32
      bytes. *)
  val create : ?previous:(string * string) list -> current:string * string -> unit -> t

  (** The id of the key new tokens are signed with. *)
  val current_id : t -> string
end

(** What a token says. *)
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

(** Why a token was refused. *)
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

(** Session settings: keys, lifetimes and the revocation hook. *)
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

(** {1 Server-side sessions in a cookie}

    The mode above keeps nothing on the server and suits API clients that send
    [Authorization: Bearer]. Browsers are better served by an opaque token in an
    [HttpOnly] cookie, which scripts on the page cannot read, with the session record kept
    on the server: logging out deletes the record, and nothing about the session travels
    in the token.

    {!Server_side} issues 32 random bytes in URL-safe base64 as the token and stores the
    session's {!Claims.t} under the token's SHA-256, so a copy of the session table holds
    no usable tokens. The middleware binds the same claims as {!middleware}, so {!claims}
    and step-up grants work the same in both modes.

    Cookies are sent by the browser on cross-site requests too, within the limits of
    [SameSite]. With the default [SameSite=Lax], a cross-site POST carries no cookie; with
    [No_restriction], protect unsafe requests against CSRF yourself (for example, check
    [Origin]). *)

(** The session cookie's name and attributes. *)
module Cookie : sig
  (** The [SameSite] attribute. *)
  module Same_site : sig
    type t =
      | Strict (** never sent on cross-site requests *)
      | Lax (** sent on top-level cross-site navigations with safe methods only *)
      | No_restriction (** [SameSite=None]: always sent; needs [Secure] *)
    [@@deriving sexp, compare, equal]
  end

  type t [@@deriving sexp_of]

  (** Defaults: name ["__Host-session"], [Secure], [HttpOnly], [SameSite=Lax], [Path=/]
      and no [Domain]. Raises (a mistake in the program, not in its input) when the name
      is not an RFC 6265 token, the path does not start with [/] or holds [;] or white
      space, a [__Host-] name lacks [Secure] or has a [Domain] or another path, a
      [__Secure-] name lacks [Secure], or [No_restriction] lacks [Secure]. Browsers drop
      cookies that break the prefix rules, so they are checked here rather than discovered
      there. *)
  val create
    :  ?name:string
    -> ?secure:bool
    -> ?http_only:bool
    -> ?same_site:Same_site.t
    -> ?path:string
    -> ?domain:string
    -> unit
    -> t

  (** The cookie's name. *)
  val name : t -> string

  (** A [Set-Cookie] header (name and value) storing [value] for [max_age], rounded up to
      whole seconds ([Max-Age] only, no [Expires]). *)
  val set : t -> value:string -> max_age:Time_ns.Span.t -> string * string

  (** A [Set-Cookie] header that removes the cookie ([Max-Age=0], empty value). *)
  val clear : t -> string * string

  (** The cookie's value in the request's [Cookie] headers; the first non-empty one when
      it appears more than once. *)
  val find : t -> Request.t -> string option

  (** The pairs of one [Cookie] header value, in order, double quotes around a value
      removed. Pairs without [=] are skipped. *)
  val parse_header : string -> (string * string) list
end

(** Where server-side session records live, keyed by the hex SHA-256 of their token. A
    service with several instances implements it over its database. *)
module type Store = sig
  type t

  (** Stores a new record; an existing [token_hash] is an error. *)
  val insert : t -> token_hash:string -> Claims.t -> unit Or_error.t Async.Deferred.t

  (** The record stored under [token_hash]. *)
  val find : t -> token_hash:string -> Claims.t option Or_error.t Async.Deferred.t

  (** Removes the record under [token_hash], if any. *)
  val remove : t -> token_hash:string -> unit Or_error.t Async.Deferred.t

  (** Removes every record of a session ("log out everywhere"); returns how many. *)
  val remove_session : t -> session_id:string -> int Or_error.t Async.Deferred.t

  (** Removes records that expired before [before]; returns how many. *)
  val purge : t -> before:Time_ns.t -> int Or_error.t Async.Deferred.t
end

(** Records in a hash table, for one instance and for tests. *)
module Memory_store : sig
  include Store

  (** An empty store. *)
  val create : unit -> t

  (** Records currently held. *)
  val length : t -> int
end

(** Opaque tokens backed by a {!Store}, carried in a cookie (and, optionally, as a bearer
    token). Store failures come back as [Error] in the outer [Or_error.t]; the inner
    result is the verdict on the token. *)
module Server_side (Store : Store) : sig
  type t

  (** [ttl] (default 24 hours) and [max_lifetime] (default 30 days) mean what they mean
      for {!create}; [cookie] defaults to [Cookie.create ()]; [random] (default: the
      system's secure source) produces tokens and ids. *)
  val create
    :  ?ttl:Time_ns.Span.t
    -> ?max_lifetime:Time_ns.Span.t
    -> ?random:(int -> string)
    -> ?cookie:Cookie.t
    -> store:Store.t
    -> clock:Clock.t
    -> unit
    -> t

  (** The cookie settings. *)
  val cookie : t -> Cookie.t

  (** A new session for [subject]: the token and its stored claims. *)
  val issue : t -> subject:string -> (string * Claims.t) Or_error.t Async.Deferred.t

  (** The claims of a stored, unexpired token. A token of the wrong shape is [Malformed]
      and one the store does not hold (never issued, rotated away or revoked) is
      [Revoked]; both render as 401 [session_invalid]. *)
  val verify : t -> string -> (Claims.t, Error.t) Result.t Or_error.t Async.Deferred.t

  (** A new token in the same session, as {!rotate} does for signed tokens; the old token
      is removed at once. *)
  val rotate
    :  t
    -> string
    -> (string * Claims.t, Error.t) Result.t Or_error.t Async.Deferred.t

  (** Removes one token (log out). Unknown or malformed tokens are ignored. *)
  val revoke : t -> string -> unit Or_error.t Async.Deferred.t

  (** Removes every token of a session; returns how many. *)
  val revoke_session : t -> session_id:string -> int Or_error.t Async.Deferred.t

  (** The [Set-Cookie] header for an issued or rotated token, lasting until the claims
      expire. *)
  val set_cookie : t -> string * Claims.t -> string * string

  (** The [Set-Cookie] header that removes the cookie, for log-out responses. *)
  val clear_cookie : t -> string * string

  (** The token in the request's cookie. *)
  val token : t -> Request.t -> string option

  (** Authenticates requests by the session cookie and binds the claims for {!claims}.
      With [bearer], a request without the cookie may present the token as
      [Authorization: Bearer] instead, and refusals carry [WWW-Authenticate: Bearer].
      Without a token: 401 [session_required], or pass through with [optional]. An invalid
      token: {!Error.to_problem}, and a refused cookie is cleared with [Set-Cookie]. A
      store failure: 503 [session_unavailable]. *)
  val middleware : ?optional:bool -> ?bearer:bool -> t -> Router.Middleware.t

  (** Removes expired records; run it periodically. *)
  val purge : t -> int Or_error.t Async.Deferred.t
end
