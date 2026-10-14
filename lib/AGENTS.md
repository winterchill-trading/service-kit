# AGENTS.md: service-kit/lib

The `service-kit` library. One module per concern, each with an `.mli` and odoc on every
public value. `service_kit.ml` re-exports the modules under the `Service_kit` name.

Build from the repository root: `opam exec --switch 5.2.0+ox -- dune build --root .. @service-kit/all`.

Rules that matter here:

- Each module is independent: no application types, no database. A module that keeps state
  (idempotency records, used step-up grants) takes a module type, with an in-memory
  implementation next to it.
- Time enters as a `Clock.t` argument. Do not read the wall clock in this directory.
- Do not `open Async` in a file that uses the `Clock` module (see the root `AGENTS.md`).
- A new module needs: the `.ml` and `.mli`, expect tests in `test/`, a re-export in
  `service_kit.ml`, and a row in the README module table.
