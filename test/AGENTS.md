# AGENTS.md: service-kit/test

Inline tests for every module of the library: ppx_expect (`-source-tree-root .`) and
Quickcheck properties. Shared generators live in `gens.ml`.

Run from the repository root: `opam exec --switch 5.2.0+ox -- dune build --root .. @service-kit/runtest`.

Rules that matter here:

- Promote expect output with `dune promote` only after reading the diff; a promoted
  output is the new expected behaviour.
- Async tests go in their own file. `test_server.ml` is the only test that binds a real
  port; other tests stay off the network.
- Never print a `Time_ns.t` sexp in an expect block (its text depends on the zone). Use
  `Time_ns.Alternate_sexp.t` or print the epoch value.
- A changed bin_prot digest in a `Stable` test means a new stable version (see the root file).
