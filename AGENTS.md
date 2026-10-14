# AGENTS.md: service-kit

## Purpose

service-kit holds the building blocks for Async HTTP services on cohttp-async: typed routes,
RFC 9457 problem responses, idempotency keys, signed sessions, step-up grants, rate limits,
request logs, health and readiness, Prometheus metrics, CORS and cursor pagination. Each
module is independent, keeps no state of its own and is tested without a database.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `service-kit` library (module `Service_kit`): one module per concern, each with an `.mli` |
| `test/` | Inline expect tests and Quickcheck properties per module (`test_session_cookie.ml` for server-side sessions, `test_idempotency_failures.ml` for raising handlers and failing stores), `gens.ml` (shared generators) |
| `examples/` | `quick_start.ml` (smallest service), `transfers.ml` (a fuller JSON API), `transfers_service.ml` (its server), `test_transfers.ml` (end-to-end test) |
| `docs/` | `api-notes.md`: the standards and libraries behind the modules |
| `.github/` | `workflows/ci.yml` |
| `README.md` | Module table, problem codes, usage of each module, known gaps |

`CHANGES.md` keeps the Keep a Changelog notes under `Unreleased`. `service-kit.opam` is
generated from `dune-project`.

## Build, test, docs

Run from the repository root, inside the workspace (`..` is the workspace root):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @service-kit/all
opam exec --switch 5.2.0+ox -- dune build --root .. @service-kit/runtest
```

For parallel work, give each agent its own build directory, for example
`DUNE_BUILD_DIR=_build-service-kit-<task>`.

Standalone (outside the workspace): `opam install . --deps-only --with-test`, then
`dune build && dune test`. Every dependency is a public opam package.

## Conventions

- Format only this repository: `dune build --root .. @service-kit/fmt --auto-promote`, after
  the tests pass. Never run `dune fmt` at the workspace root.
- Every module has an `.mli`. Use `open! Core`, labelled arguments and `ppx_jane`.
- Time is an argument. Modules that need it take a `Clock.t` (`Clock.wall` in a service, a
  manual clock in tests); none reads the wall clock.
- `Stable` modules (`Param`, `Message`, `Idempotency`, `Step_up`, `Pagination`) have pinned
  bin_prot digests. A change to a pinned shape needs a new stable version, not an edit.
- Tests: ppx_expect with `-source-tree-root .`, plus Quickcheck properties. In `test/`, only
  `test_server.ml` uses real HTTP.
- odoc on every public value. User-visible changes get a line under `Unreleased` in `CHANGES.md`.

## Dependencies and dependents

- No sibling repository is built against. Dependencies are public packages: core, async and
  ppx_jane (v0.17 up to v0.19), cohttp-async and cohttp (6.0 or later), http, uri, yojson
  (2.2 up to 4.0), cryptokit (1.16 or later); core_unix and expect_test_helpers_core for tests.
- Dependents: `../auth-kit` uses service-kit in its tests (`service-kit :with-test`).
- CI pins nothing: `ox-setup` runs without a `pins:` block.

## CI

`.github/workflows/ci.yml` runs on every push and pull request. It runs the `ox-setup`
composite action from `winterchill-trading/infra`, then `dune build`, `dune build @examples/all`
and `dune test`, then the `obj-magic` reusable workflow from the same infra repository, which
flags `Obj.magic` and `Obj.repr`. To check a run, use `gh run list --limit 1`, and at most one
`gh run watch <id> --exit-status`. Do not poll in a loop.

## Pitfalls

- `open! Async` brings `Async.Clock` into scope and shadows the library's `Clock`. In library
  files, skip `open Async` or alias the module first.
- `Cohttp.Code.status_code` derives only `sexp`. Compare statuses through `code_of_status`
  in a local module.
- `Time_ns.t` sexps print in the machine's zone, so expect output differs between a Mac and CI.
  Use `Time_ns.Alternate_sexp.t` in unstable types and keep `Stable.V1` for bin_io.
- `Response.render_problem` adds the problem's own headers (`Allow`, `Retry-After`) to a
  custom rendering. Hooks that render refusals should go through it.
- `Session.Cookie.create` raises on settings browsers silently drop (`__Host-` without
  `Secure`, `SameSite=None` without `Secure`). Tests that need a plain cookie pass
  `~name` and `~secure:false` together.
- An idempotency layer must never store 401 or 403 answers, or a retry that carries a valid
  step-up grant replays the refusal.
- `Step_up` in binding mode reports a grant bound to another request as `Already_used`;
  `Error.t` gained no constructor, so existing matches stay exhaustive. `consume` in that
  mode binds to a random one-off request, so it remains single use.
- Record fields with one name in sibling types resolve to the last type defined. Annotate
  the binding.
- Async expect tests go in their own file; mixing them with a pure test fails to type-check.
- `dune promote` after a raising test writes an `[@@expect.uncaught_exn]` block. Delete it
  once the test is fixed.

## Where to read more

- `README.md` (module table, problem codes, idempotency, sessions, known gaps).
- `docs/api-notes.md` (RFC 9457, the IETF Idempotency-Key draft, Prometheus, CORS, GCRA).
- `examples/transfers.ml`, a full service built on the kit.
- `../oxcaml-claude-skills/AGENTS.md` (workspace guide), the skills
  `../oxcaml-claude-skills/skills/async-http-websocket/` and
  `../oxcaml-claude-skills/skills/janestreet-ocaml-style/`, and the section "service-kit" of
  `../oxcaml-claude-skills/GOTCHAS.md`.

## Never do

- Install an opam package that changes the compiler, `core`, `base`, `async` or `ppx_jane` on
  the shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Publish, tag or release.
- Commit credentials, session keys or tokens, including in test fixtures.
