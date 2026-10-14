# AGENTS.md: service-kit/examples

Runnable examples built with the library and not installed.

- `quick_start.ml`: one typed route with request logs and metrics, about 40 lines.
- `transfers.ml`: the domain of a small JSON API (accounts, sign-in, passcode step-up,
  idempotent transfers, cursor pages). `transfers_service.ml` serves it; `test_transfers.ml`
  drives it end to end with the examples' own tests.

Build from the repository root: `opam exec --switch 5.2.0+ox -- dune build --root .. @service-kit/all`.
CI builds the examples on their own with `dune build @examples/all`.

Keep the examples compiling when the library's signatures change; they are the first
thing a new service author copies.
