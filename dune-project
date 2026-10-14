(lang dune 3.17)

(name service-kit)

(generate_opam_files true)

(license MIT)

(maintainers "hello@winterchill.xyz")

(authors "WinterChill AI")

(source
 (github winterchill-trading/service-kit))

(homepage "https://github.com/winterchill-trading/service-kit")

(bug_reports "https://github.com/winterchill-trading/service-kit/issues")

(documentation "https://github.com/winterchill-trading/service-kit#readme")

(package
 (name service-kit)
 (synopsis "Building blocks for Async HTTP services on cohttp-async")
 (description
  "service-kit collects the pieces most JSON APIs on Async need: typed routes with a middleware chain, RFC 9457 problem responses with stable codes, structured messages for client-side localisation, idempotency keys with stored-response replay, HMAC-signed sessions, single-purpose step-up grants, token-bucket rate limits, structured request logs with redaction, health and readiness endpoints, Prometheus metrics, CORS and cursor pagination. It has no database dependency: everything that keeps state does so behind a small module type with an in-memory implementation.")
 (tags
  (http server async cohttp idempotency))
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (core
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (async
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (cohttp-async
   (>= 6.0.0))
  (cohttp
   (>= 6.0.0))
  (http
   (>= 6.0.0))
  (uri
   (>= 4.4.0))
  (yojson
   (and
    (>= 2.2.0)
    (< 4.0)))
  (cryptokit
   (>= 1.16))
  (odoc :with-doc)))
