# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

First release.

### Added

- `Router`: typed paths with literal and parameter segments, method dispatch with 404
  and 405 (`Allow`), global and per-route middleware; global middleware sees the route
  pattern in `Request.route`.
- `Request` and `Response`: decoded path segments, JSON bodies with problem errors, a
  context map for middleware, JSON/text/problem responses.
- `Server`: cohttp-async adapter with a body limit (413), exceptions answered 500 and
  reported to `on_exn`, and the peer address on the request.
- `Problem`: RFC 9457 problem documents with validated codes and typed params; helpers
  for common statuses including 429 with `Retry-After`.
- `Message` and `Message.Catalogue`: structured messages, catalogues with parameter
  kinds and per-locale templates, validation, coverage checks and rendering.
- `Param`: typed values (string, int, bool, decimal, time, list) with JSON and stable
  bin_prot encodings.
- `Idempotency`: `Idempotency-Key` middleware with stored-response replay, 409 on reuse
  with a different request or while in progress, retention, stale-record takeover and a
  `Store` module type with `Memory_store`.
- `Session`: HMAC-SHA256 tokens with a keyring for key rotation, token rotation up to a
  maximum lifetime, a revocation hook and a middleware exposing the claims.
- `Step_up`: single-purpose, session-bound grants with a 5-minute default lifetime,
  `check`, single-use `consume` through a `Spent` store, and `require` middleware.
- `Rate_limit`: token buckets (GCRA) with integer arithmetic, bounded key tables and a
  429 middleware.
- `Request_log`: per-request entries with request ids, handler fields, header and query
  redaction, JSON and sexp output.
- `Health`: `/healthz` and `/readyz` with concurrent checks, timeouts, non-critical
  checks and draining.
- `Metrics`: Prometheus counters, gauges, computed gauges and histograms, per-route
  request metrics middleware and a `/metrics` route.
- `Cors`: origin allow lists, preflight handling and exposed headers.
- `Pagination`: versioned, optionally signed cursors over any bin_prot position,
  `limit` parsing and pages from an over-fetch.
- `Clock`: injectable time with a manual clock.
- Examples: `quick_start` and a transfers service with end-to-end expect tests.
- `Idempotency`: a handler that raises never leaves its key in progress. The key is
  released by default, or completed with a chosen response with
  `~on_handler_exn:(Record stored)`; a store that raises counts as a store error.
- `Idempotency.Key_policy`: `strict` (the default, unchanged) and `permissive`, which
  accepts any non-empty UTF-8 key up to a byte length; passed as `~key_policy`.
- `Idempotency.Replayed_header`: with `~replayed_header:Always`, every response to an
  unsafe request carries `Idempotent-Replayed`, `false` unless it is a replay.
- `Response.render_problem`, and a `~render_problem` hook on `Idempotency.Make.create`
  and `Router.create`: the kit's own refusals (400, 409 and 503 from idempotency, 404
  and 405 from the router) can use a service's error body; problem headers such as
  `Allow` and `Retry-After` are kept.
- `Session.Server_side`, `Session.Cookie`, `Session.Store` and `Session.Memory_store`:
  opaque server-side session tokens carried in a cookie (configurable name, `Secure`,
  `HttpOnly`, `SameSite`, path and domain, with the `__Host-` and `__Secure-` prefix
  rules checked), stored by SHA-256, with rotation, revocation, "log out everywhere",
  purge and a middleware that can also accept the token as a bearer token.
