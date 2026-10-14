# Notes on the standards and libraries behind service-kit

## Problem details (RFC 9457)

- Media type `application/problem+json`. Members: `type` (URI, default
  `about:blank`), `title`, `status`, `detail`, `instance`, plus extensions.
- When `type` is `about:blank`, `title` should be the HTTP reason phrase. service-kit
  always uses the reason phrase as `title` and never emits `detail`, so no response
  text is chosen by the server; `code` and `params` are extension members.
- `instance` is free for a request reference; `Problem.with_instance` sets it.

## Idempotency-Key (IETF httpapi draft)

- The header value is an opaque string; the draft recommends UUIDs. service-kit
  accepts 1-255 visible ASCII characters and answers 400 otherwise.
- The draft's error statuses: 400 when a required key is missing, 409 when a request
  with the same key is still being processed, 422 when the key is reused with a
  different payload. service-kit answers 409 for reuse as well (`idempotency_key_reused`),
  which reads as "this conflicts with what the key already means"; clients tell the two
  409s apart by `code`.
- The fingerprint covers method, path with query and the body. JSON bodies are
  canonicalised (object keys sorted, whitespace dropped) so equivalent payloads match;
  array order still matters.
- Responses are stored only when they are final for that request: 5xx, 401, 403, 409
  and 429 release the key.

## Prometheus text format 0.0.4

- `# HELP` text escapes `\` and newline; label values escape `\`, `"` and newline.
- Histograms expose cumulative `_bucket{le="..."}` series ending with `le="+Inf"`
  (equal to `_count`), then `_sum` and `_count`.
- Metric names match `[a-zA-Z_:][a-zA-Z0-9_:]*`; label names `[a-zA-Z_][a-zA-Z0-9_]*`
  and names starting with `__` are reserved.
- Content type `text/plain; version=0.0.4; charset=utf-8`.

## CORS

- A preflight is `OPTIONS` with `Origin` and `Access-Control-Request-Method`; the
  answer needs `Access-Control-Allow-Origin`, `-Methods`, `-Headers` and optionally
  `-Max-Age`. Browsers reject `Access-Control-Allow-Origin: *` together with
  `Access-Control-Allow-Credentials: true`, so `Cors.create` refuses that combination.
- Responses that vary by `Origin` need `Vary: Origin` or shared caches can serve one
  origin's answer to another.

## Token buckets as GCRA

- A bucket is one timestamp, the theoretical arrival time (TAT). A request costing `c`
  is allowed when `max(TAT, now) + c * T - now <= burst * T`, and then
  `TAT := max(TAT, now) + c * T`. This is equivalent to a token bucket of size `burst`
  refilled every `T`, needs no floating point and gives the exact wait for
  `Retry-After`.

## cohttp-async and the OxCaml switch

- `Cohttp.Code.status_code` derives only sexp; compare statuses through
  `Cohttp.Code.code_of_status`.
- `Cohttp.Code.meth` has no `equal`; compare `string_of_method`.
- `Cohttp_async.Server.listening_on` returns the bound port when listening on port 0.
- `open Async` brings `Async.Clock` into scope, which shadows a library module named
  `Clock`; modules that use both keep this library's under another name.
- `Array.binary_search` returns a local option under OxCaml, which cannot escape into a
  global value; short bucket arrays use a linear scan instead.
- cryptokit 1.16 provides HMAC-SHA256, SHA-256, `string_equal` (constant time) and a
  secure RNG, but no AEAD cipher.
