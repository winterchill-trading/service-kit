(** Building blocks for Async HTTP services on cohttp-async.

    {2 Serving}
    - {!Router}: typed routes, method dispatch and middleware.
    - {!Request}, {!Response}: what handlers see and return.
    - {!Server}: the cohttp-async adapter (body limits, exceptions as 500).

    {2 Responses for clients}
    - {!Problem}: RFC 9457 problem documents with stable codes and typed params.
    - {!Message}: structured messages and a catalogue for client-side localisation.
    - {!Param}: the typed values both carry.
    - {!Pagination}: opaque cursors and pages.

    {2 Safety}
    - {!Idempotency}: [Idempotency-Key] with stored-response replay.
    - {!Session}: HMAC-signed session tokens with key and token rotation, or server-side
      sessions in a cookie.
    - {!Step_up}: single-purpose, short-lived, session-bound grants.
    - {!Rate_limit}: token buckets per key.
    - {!Cors}: cross-origin rules and preflight.

    {2 Operations}
    - {!Request_log}: one structured entry per request, with request ids and redaction.
    - {!Health}: [/healthz] and [/readyz].
    - {!Metrics}: Prometheus counters, gauges and histograms.
    - {!Clock}: injectable time for all of the above.

    Nothing here touches a database: modules that keep state ({!Idempotency}, {!Step_up},
    server-side {!Session}s) do so behind a small module type with an in-memory
    implementation. *)

module Clock = Clock
module Codec = Codec
module Cors = Cors
module Health = Health
module Idempotency = Idempotency
module Message = Message
module Metrics = Metrics
module Pagination = Pagination
module Param = Param
module Problem = Problem
module Rate_limit = Rate_limit
module Request = Request
module Request_log = Request_log
module Response = Response
module Router = Router
module Server = Server
module Session = Session
module Step_up = Step_up
