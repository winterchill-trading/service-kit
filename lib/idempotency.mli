(** Idempotency keys for unsafe requests (draft-ietf-httpapi-idempotency-key-header).

    A client that sends [Idempotency-Key: <key>] with a POST, PUT, PATCH or DELETE can
    retry after a lost response without doing the work twice:

    - the first request with a key runs and its response is stored;
    - a retry with the same key and the same request gets the stored response again,
      marked [Idempotent-Replayed: true];
    - the same key with a different request (method, path, query or body) is refused with
      409 [idempotency_key_reused];
    - a retry while the first is still running is refused with 409
      [idempotency_key_in_use] and [Retry-After: 1].

    Keys are scoped by caller, method and route pattern, so two users cannot collide and
    one user's key cannot replay another's response. Records are kept for [retention] (24
    hours by default); after that the key is free again. Bodies are compared by a SHA-256
    digest of their canonical JSON (object keys sorted), or of the raw bytes when they are
    not JSON, so key order and whitespace do not count as a different request.

    Storage is a small {!Store} module type; {!Memory_store} keeps records in process. A
    service with several instances implements {!Store} over its database: [claim] must be
    atomic (an insert that fails on an existing key, e.g. [INSERT ... ON CONFLICT]).

    If storing the finished response fails, the record stays in progress: retries are
    refused until [stale_after] has passed, then the request runs again. Handlers that
    move money should also pass the key on to their providers so a second run finds the
    first one's result. *)

open! Core

(** What a key is unique within. *)
module Scope : sig
  type t =
    { caller : string (** e.g. the session subject *)
    ; meth : string
    ; route : string (** the route pattern *)
    ; key : string
    }
  [@@deriving sexp, compare, equal, hash]
end

(** A response as stored for replay. *)
module Stored : sig
  type t =
    { status : int
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp, compare, equal]
end

(** One key's record. *)
module Record : sig
  type state =
    | In_progress
    | Completed of Stored.t
  [@@deriving sexp, compare, equal]

  type t =
    { scope : Scope.t
    ; request_digest : string
    ; created_at : Time_ns.t
    ; state : state
    }
  [@@deriving sexp, compare, equal]
end

(** Where records live. *)
module type Store = sig
  type t

  (** Stores [record] (always [In_progress]) and answers [`Claimed] when there is no
      record for its scope, or the existing one was created before [reclaim_before] (past
      retention), or it is still [In_progress] and was created before [stale_before] (its
      request was abandoned). Otherwise leaves the store alone and answers [`Exists] with
      the existing record. Must be atomic. *)
  val claim
    :  t
    -> Record.t
    -> reclaim_before:Time_ns.t
    -> stale_before:Time_ns.t
    -> [ `Claimed | `Exists of Record.t ] Or_error.t Async.Deferred.t

  (** Marks the scope's record [Completed]. *)
  val complete : t -> Scope.t -> Stored.t -> unit Or_error.t Async.Deferred.t

  (** Removes the scope's record, so the key can be used again (after a failure that
      should not be replayed). *)
  val release : t -> Scope.t -> unit Or_error.t Async.Deferred.t

  (** Removes records created before [before]; returns how many. *)
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

(** The request header, ["idempotency-key"]. *)
val header_name : string

(** Whether a key is acceptable: 1 to 255 visible ASCII characters. *)
val valid_key : string -> bool

(** The digest that tells two requests apart: method, path and query, and the body as
    canonical JSON (or raw bytes when it is not JSON). *)
val request_digest : Request.t -> string

(** The middleware over a given store. *)
module Make (Store : Store) : sig
  type t

  (** [retention] defaults to 24 hours and [stale_after] to 5 minutes. Responses whose
      status passes [store_status] are stored; others release the key so the client can
      retry (default: everything below 500 except 401 and 403, which a retry with new
      credentials may fix, and 409 and 429, which describe momentary state). [caller]
      names who is asking (default: a SHA-256 of the [Authorization] header, or
      ["anonymous"]); put the middleware after authentication and pass the authenticated
      subject. With [required], unsafe requests without a key get 400
      [idempotency_key_missing]. [on_store_error] sees store failures (default: ignored);
      a failed claim answers 503 [idempotency_unavailable] without running the handler. *)
  val create
    :  ?retention:Time_ns.Span.t
    -> ?stale_after:Time_ns.Span.t
    -> ?store_status:(int -> bool)
    -> ?caller:(Request.t -> string)
    -> ?required:bool
    -> ?on_store_error:(Error.t -> unit)
    -> store:Store.t
    -> clock:Clock.t
    -> unit
    -> t

  (** Applies the rules above to POST, PUT, PATCH and DELETE requests that carry a key
      (400 [invalid_idempotency_key] for a malformed one); other requests pass through. *)
  val middleware : t -> Router.Middleware.t

  (** Removes records past retention; run it periodically. *)
  val purge : t -> int Or_error.t Async.Deferred.t
end

(** Bin_prot shapes for stores that serialise records. *)
module Stable : sig
  module Record : sig
    module V1 : sig
      type t = Record.t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end
