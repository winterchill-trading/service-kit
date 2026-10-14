# service-kit

Building blocks for Async HTTP services written in the Jane Street style, on top of
cohttp-async.

| Module | What it does |
|--------|--------------|
| `Router` | Typed routes (`Path.(root / "accounts" /: string "id")` gives the handler a `string` argument), method dispatch with 404/405 (body renderable by a hook), a middleware chain |
| `Request`, `Response` | The request with its body read and context for middleware; JSON, text and problem responses |
| `Server` | cohttp-async adapter: body size limit (413), exceptions as 500, peer address |
| `Problem` | RFC 9457 `application/problem+json` with stable codes and typed params, never free text |
| `Message` | Structured messages (code + typed params) and a catalogue with per-locale templates and coverage checks |
| `Param` | The typed values problems and messages carry (string, int, bool, decimal, time, list) |
| `Idempotency` | `Idempotency-Key` per caller, method and route; stored-response replay; 409 on reuse with another body; retention; pluggable store |
| `Session` | Opaque HMAC-SHA256 tokens, key rotation through a keyring, token rotation up to a maximum lifetime, revocation hook; or server-side sessions in an `HttpOnly` cookie over a pluggable store |
| `Step_up` | Single-purpose, time-bound, session-bound grants; issue, check, consume once; purposes are the application's variant |
| `Rate_limit` | Token buckets per key, exact integer arithmetic, time passed in; middleware answering 429 with `Retry-After` |
| `Request_log` | One structured entry per request, request id propagation (`X-Request-Id`), header and query redaction |
| `Health` | `/healthz` and `/readyz` with concurrent dependency checks, timeouts and draining |
| `Metrics` | Prometheus text exposition: counters, gauges, histograms, per-route request metrics |
| `Cors` | Allowed origins, preflight answered before routing, `Vary: Origin` |
| `Pagination` | Opaque (optionally signed) keyset cursors, `limit` parsing, pages from an over-fetch |
| `Clock` | Injectable time; a manual clock for tests |

## Why separate

Every JSON API ends up writing the same plumbing: replaying a payment request whose
response was lost, refusing a request that reuses a key with another body, telling a
client "insufficient funds, 12.50 available" in a way it can translate, signing session
tokens, asking for a passcode again before money moves, counting requests by route
without letting clients create label values, and answering health probes. Written
inside an application, each piece grows its own corner cases and is tested only through
the application. Here each is a small module with its own expect tests and properties,
no application types and no database: anything that keeps state (idempotency records,
used step-up grants) does so behind a module type with an in-memory implementation, and
a service supplies the persistent one.

## Quick start

```ocaml
open! Core
open! Async
open Service_kit

let hello name (_ : Request.t) =
  if String.length name > 64
  then
    return (Response.problem (Problem.bad_request "name_too_long" ~params:[ "max", Int 64 ]))
  else
    return
      (Response.json
         (Message.to_yojson (Message.create "greeting" ~params:[ "name", String name ])))
;;

let main () =
  let registry = Metrics.create () in
  let requests = Metrics.Requests.create registry in
  let log entry = print_endline (Yojson.Safe.to_string (Request_log.Entry.to_yojson entry)) in
  let router =
    Router.create
      ~middleware:
        [ Request_log.middleware ~clock:Clock.wall ~sink:log ()
        ; Metrics.Requests.middleware requests ~clock:Clock.wall
        ]
      (Router.Route.get Router.Path.(root / "hello" /: string "name") hello
       :: Metrics.route registry
       :: Health.routes (Health.create []))
  in
  let%bind server = Server.start ~port:8080 (Router.handle router) in
  printf "listening on %d\n" (Cohttp_async.Server.listening_on server);
  Deferred.never ()
;;
```

```
$ curl -s localhost:8080/hello/world
{"code":"greeting","params":{"name":"world"}}
```

This is `examples/quick_start.ml`. `examples/transfers.ml` is a fuller service: sign-in,
passcode step-up, idempotent transfers with problem responses, cursor pages, health,
metrics, CORS, rate limits and request logs, driven end to end by
`examples/test_transfers.ml`.

## Responses: problems and messages

Errors are RFC 9457 problem documents. The `code` is the contract; `title` is the
status's reason phrase and there is no `detail`, so the server never chooses the user's
language:

```json
{"type":"about:blank","title":"Conflict","status":409,"code":"insufficient_funds",
 "params":{"available":"87.50"}}
```

Successful actions that need a sentence return a `Message` (`{"code": "transfer_sent",
"params": {...}}`). A `Message.Catalogue` lists each code's parameters with their kinds
and a template per locale; `Message.Catalogue.check ~codes ~locales` in a test fails on
any code without an entry, any entry without a template for a locale, and (by default)
any entry nobody emits. `Message.Catalogue.to_yojson` ships the catalogue to clients.

Codes the library itself emits:

| Code | Status | Module |
|------|--------|--------|
| `not_found` | 404 | `Router` |
| `method_not_allowed` | 405 | `Router` (with `Allow`) |
| `payload_too_large` (`max_bytes`) | 413 | `Server` |
| `internal_error` | 500 | `Server`, `Step_up` store failures |
| `invalid_json`, `invalid_body` | 400 | `Request.json`, `Request.decode` |
| `idempotency_key_missing`, `invalid_idempotency_key` | 400 | `Idempotency` |
| `idempotency_key_reused`, `idempotency_key_in_use` | 409 | `Idempotency` |
| `idempotency_unavailable` | 503 | `Idempotency` |
| `session_required`, `session_invalid`, `session_expired` | 401 | `Session` (with `WWW-Authenticate: Bearer`) |
| `step_up_required` (`purpose`) | 403 | `Step_up` |
| `rate_limited` (`retry_after_seconds`) | 429 | `Rate_limit` (with `Retry-After`) |
| `invalid_cursor`, `invalid_limit` (`max`) | 400 | `Pagination` |

## Idempotency

```ocaml
module Idem = Idempotency.Make (Idempotency.Memory_store)

let idem =
  Idem.create
    ~caller:(fun req -> (Option.value_exn (Session.claims req)).subject)
    ~store:(Idempotency.Memory_store.create ())
    ~clock:Clock.wall
    ()

(* route middleware, after authentication *)
Route.post Path.(root / "transfers") ~middleware:[ Session.middleware sessions; Idem.middleware idem ] send
```

- Same key, same request (method, path, query and canonical JSON body): the stored
  response again, with `Idempotent-Replayed: true`.
- Same key, different request: 409 `idempotency_key_reused`.
- Same key while the first is running: 409 `idempotency_key_in_use`, `Retry-After: 1`.
- 5xx, 401, 403, 409 and 429 answers are not stored, so a retry can succeed.
- Records expire after `retention` (24 h); `Idem.purge` removes them.
- A handler that raises never leaves its key in progress: the record is removed, or,
  with `~on_handler_exn:(Record stored)`, completed with `stored` so retries replay it.

Options on `Idem.create`:

| Option | Default | Effect |
| --- | --- | --- |
| `~key_policy` | `Key_policy.strict` (1-255 visible ASCII) | `Key_policy.permissive ~max_bytes ()` accepts any non-empty UTF-8 |
| `~replayed_header` | `On_replay` | `Always` adds `Idempotent-Replayed: false` to every other unsafe response |
| `~render_problem` | problem+json | renders the 400, 409 and 503 refusals in the service's own error body |
| `~on_handler_exn` | `Release` | `Record stored` replays `stored` after a handler raised |

`Router.create ~render_problem` does the same for 404 and 405. Headers the problem carries
(`Allow`, `Retry-After`) are added to the rendered response when it lacks them.

To persist records, implement `Idempotency.Store` (four functions: an atomic `claim`,
`complete`, `release`, `purge`) over your database; `Idempotency.Stable.Record.V1` gives
a bin_prot shape if you store records as blobs.

## Sessions and step-up

```ocaml
let sessions =
  Session.create
    ~keyring:(Session.Keyring.create ~current:("2026-10", key) ~previous:[ "2026-09", old_key ] ())
    ~is_revoked:(fun claims -> Revocations.mem claims.session_id)
    ~clock:Clock.wall
    ()

module Purpose = struct
  type t = Send_payment | Reveal_card [@@deriving sexp, compare, equal]
  let to_string = function Send_payment -> "send_payment" | Reveal_card -> "reveal_card"
  let of_string = function "send_payment" -> Some Send_payment | "reveal_card" -> Some Reveal_card | _ -> None
end

module Grants = Step_up.Make (Purpose) (Step_up.Memory_spent)

(* after the user re-entered a passcode: *)
let grant, _ = Grants.issue grants ~session_id:claims.session_id ~purpose:Send_payment

(* on the route that moves money: *)
Grants.require grants ~purpose:Send_payment ~session_id:(fun req ->
  Option.map (Session.claims req) ~f:(fun c -> c.session_id))
```

Tokens are `v1.<key id>.<claims>.<mac>`; grants are `su1.<claims>.<mac>`. Both are
signed, not encrypted: they carry ids and times, never personal data.

### Server-side sessions in a cookie

For browsers, `Session.Server_side` keeps the session record on the server and hands
out an opaque token (32 random bytes) in a cookie, `__Host-session` with `Secure`,
`HttpOnly` and `SameSite=Lax` by default. The store sees only the token's SHA-256.

```ocaml
module Sessions = Session.Server_side (Session.Memory_store)

let sessions =
  Sessions.create ~store:(Session.Memory_store.create ()) ~clock:Clock.wall ()

(* sign-in handler *)
let%map issued = Sessions.issue sessions ~subject:customer_id in
let token, claims = Or_error.ok_exn issued in
Response.json ~headers:[ Sessions.set_cookie sessions (token, claims) ] (`Assoc [])

(* routes *)
Route.get Path.(root / "me") ~middleware:[ Sessions.middleware sessions ] me
```

The middleware binds the same claims as the bearer mode, so `Session.claims` and step-up
grants work unchanged; `~bearer:true` also accepts the token as `Authorization: Bearer`.
`Sessions.revoke`, `revoke_session` and `clear_cookie` cover log-out. Implement
`Session.Store` (insert, find, remove, remove_session, purge) over your database for
several instances.

## Verified on

Expect tests and Quickcheck properties cover every module; `test/test_server.ml` runs
the router over real HTTP on a free port. HMAC-SHA256 is checked against RFC 4231 test
case 2 and base64url against RFC 4648 vectors. Stable bin_prot digests are pinned for
`Param`, `Message`, `Session.Claims`, `Step_up` claims and `Idempotency.Record`.

## Building

```
opam install . --deps-only --with-test
dune build && dune test
```

Requires OCaml 5.2 or later with Jane Street v0.17 or the v0.18 previews, cohttp-async
6, cryptokit 1.16 and yojson 2. CI builds with OxCaml.

## Used by

No other org repository depends on service-kit yet.

## Known gaps

- No websocket helpers; `Server.to_cohttp` serves plain HTTP responses only (use
  `Cohttp_async.Server.create_expert` alongside it for upgrades).
- No persistent `Idempotency.Store` or `Step_up.Spent` implementation ships here, by
  design; services write them over their own database.
- Session claims are signed but readable; there is no encrypted token format (the
  cryptokit on the switch has no AEAD cipher).
- `Session.Server_side` does not check `Origin`: with `SameSite=None`, protect unsafe
  requests against CSRF in the service.
- `Rate_limit` keeps buckets per process; several instances each enforce their own
  limit.
