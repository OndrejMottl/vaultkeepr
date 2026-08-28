# Functions, Documentation, and Tests

Behavior changes use this sequence:

1. Update the roxygen contract in the function source: purpose, parameters, return value, errors, examples, and compatibility implications.
2. Add or update focused tests for the intended behavior and run them before implementation. Confirm they fail for the intended missing behavior, not because of broken setup.
3. Implement the smallest coherent change.
4. Run `devtools::document()`; inspect changes to `NAMESPACE` and `man/`.
5. In a clean session, run `devtools::load_all()` and the focused test file, for example `testthat::test_file("tests/testthat/test-open_vault.R")`.
6. Run `devtools::test()`.
7. Run `devtools::check()`.

If a pure documentation correction cannot meaningfully fail first, state why and validate the rendered/generated output instead.

## Function contracts

- Keep one exported function per source file and a matching `test-<function>.R` file where practical.
- Validate arguments with `assertthat_cli()` and keep diagnostics specific.
- Respect `verbose` for progress messages.
- Return objects explicitly and document their classes and columns.
- Preserve lazy `vault_pipe` chains and connection ownership described in `.ai/r-coding.md`.
- Preserve existing `return_raw_data` behavior; do not add it mechanically to APIs where raw database data is not a meaningful contract.

## Tests

- Use the SQLite fixture created by `tests/testthat/helper_make_database.R`; never use a personal or live database.
- Test successful results, invalid inputs, messages/warnings/errors, empty results, and connection/query behavior relevant to the change.
- Prefer structural assertions for data frames and lazy tables: classes, names, keys, row counts, and selected representative values.
- Do not call `library(vaultkeepr)` inside individual test files; the package test bootstrap handles loading.
- Keep test data minimal, deterministic, and free of licensed or private source records.
