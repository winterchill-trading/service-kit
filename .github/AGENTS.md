# AGENTS.md: service-kit/.github

`workflows/ci.yml`: the push and pull request workflow. It runs the `ox-setup` action and
the `obj-magic` reusable workflow from `winterchill-trading/infra` (both at `@main`), with no
`pins:` block because the library builds against public packages only.

Steps in order: `dune build`, `dune build @examples/all`, `dune test`.

When a step changes, change the job that checks it in the same commit. To check a run, use
`gh run list --limit 1`, and at most one `gh run watch <id> --exit-status`.
