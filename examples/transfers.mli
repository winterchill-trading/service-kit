(** A small transfers API built from service-kit, kept in memory: sign in, step up with a
    passcode, send money idempotently, list transfers page by page. Every user starts with
    100.00. *)

open! Core

type t

(** A service. [random] makes ids and tokens (default: secure random bytes); tests pass a
    deterministic one. [log] receives request log entries (default: dropped). *)
val create
  :  ?random:(int -> string)
  -> ?log:(Service_kit.Request_log.Entry.t -> unit)
  -> clock:Service_kit.Clock.t
  -> unit
  -> t

(** The routes: [POST /sessions], [POST /step-up], [GET /balance], [POST /transfers],
    [GET /transfers], [/healthz], [/readyz], [/metrics]. *)
val router : t -> Service_kit.Router.t

(** The service's own message and problem codes, in English and German. *)
val catalogue : Service_kit.Message.Catalogue.t

(** The codes {!catalogue} must cover (library codes such as [rate_limited] are listed by
    the library's own documentation). *)
val codes : string list
