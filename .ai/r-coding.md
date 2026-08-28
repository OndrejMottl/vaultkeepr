# vaultkeepr R Coding Guidance

Canonical R guidance for package implementation, lazy VegVault queries, SQLite access, and package-facing helpers. For behavior changes, roxygen, and tests, also read `.ai/r-functions.md`; for schema-dependent work, read `.ai/database-contract.md`.

## Scope and compatibility

Apply these conventions to new and materially edited code under `R/`, `tests/testthat/`, vignette helpers, and package tooling. Preserve readable established idioms in untouched legacy sections; do not combine a functional change with an unrelated package-wide restyle.

Public function names, arguments, defaults, error conditions, return classes, columns, lazy behavior, and connection ownership are compatibility contracts. Change them only when the task explicitly authorizes an API revision and its documentation, tests, NEWS, and release implications are addressed.

## Clean package execution

Work from a clean R session:

```r
devtools::load_all()
```

Do not rely on objects, options, attached packages, connections, or environment variables left in the global session. Do not call `library(vaultkeepr)` inside package source or individual test files.

Use package-relative paths in tooling and fixtures. Never embed personal database paths, credentials, or a sibling VegVault checkout path in package code.

## File and function structure

- Keep one exported function per `R/<function_name>.R` file.
- Keep a matching `tests/testthat/test-<function_name>.R` file where practical.
- Private helpers may share a source file only when they are tightly coupled and not useful elsewhere.
- Keep database orchestration readable; extract repeated validation, mapping, or query-building logic into named private helpers.
- Source files contain functions and constants, not interactive execution.

Every exported function has a roxygen contract in its source. Follow the test-first sequence in `.ai/r-functions.md`; never hand-edit generated `NAMESPACE` or `man/` files.

## Naming

Use lower `snake_case` and descriptive full words. Function names are verbs; data objects are nouns.

Prefer type prefixes for important objects:

- `data_*`: data frames, tibbles, or lazy tables
- `table_*`: summaries intended as tables
- `list_*`: lists
- `vec_*`: vectors
- `mat_*`: matrices
- `db_con` or `con`: documented database/pipe connections
- `path_*`: file paths
- `flag_*`: logical controls
- `res_*` or `res`: returned objects

Preserve established public argument names such as `con`, `verbose`, and `return_raw_data`. Do not rename API arguments merely to satisfy a new preference.

Do not encode issue/PR numbers or temporary phase names in R identifiers, tests, fixtures, comments, or files.

Prefer a new object for a materially transformed query or data state:

```r
data_samples <-
  dplyr::tbl(db_con, "Samples")

data_samples_selected <-
  data_samples |>
  dplyr::filter(.data[["dataset_id"]] %in% vec_dataset_id)
```

Reuse a name only when the operation is intentionally in-place or the alternative would create material memory pressure.

## Formatting

- Use `<-` for assignment, two-space indentation, and `TRUE`/`FALSE`.
- Keep R and roxygen lines near 80 characters.
- Use explicit argument names when they improve clarity.
- Put one argument per line in multi-argument calls.
- Place the right-hand side on the next line after `<-` for calls, indexing, calculations, collections, and pipelines.
- Short atomic literals and direct aliases may remain on the assignment line.

```r
verbose <- TRUE

data_taxa <-
  dplyr::tbl(
    db_con,
    "SampleTaxa"
  )
```

Separate top-level executable statements within a function block with one blank line. Do not split one syntactically connected expression with decorative blank lines.

Write control-flow conditions across lines:

```r
if (
  isTRUE(verbose)
) {
  cli::cli_alert_info("Vault opened successfully")
}
```

Keep `} else {` together. Avoid one-line conditionals, assignments inside conditions, and multiple side effects in one expression.

## Namespaces and dependencies

- Use `pkg::function()` for external package calls.
- Do not call `library()` or `require()` inside package functions.
- Use imported operators only where the package already establishes them; do not attach packages to make tests pass.
- A new dependency requires a concrete package-level benefit and explicit user approval before installation, DESCRIPTION changes, or use in source.
- Prefer existing Imports before adding another package for a small convenience.

Base functions may remain unqualified in readable legacy code. Use `base::` when disambiguation matters or the surrounding code follows that convention; do not mechanically namespace every base call.

## Validation and diagnostics

Use `assertthat_cli(exp, msg, verbose)` for user-facing argument and state validation:

```r
assertthat_cli(
  is.logical(verbose) && length(verbose) == 1L && !is.na(verbose),
  msg = "{.arg verbose} must be one non-missing logical value"
)
```

- Validate cheap structural preconditions before opening connections or building queries.
- Make messages name the function argument, required class/column/table, and corrective action where useful.
- Use `cli` inline markup consistently with existing messages.
- When validating `verbose` itself, do not pass the potentially invalid value as the message gate.
- Gate informational and progress messages with `verbose`.
- Errors protecting correctness must still occur when `verbose = FALSE`.
- Do not emit messages from private helpers unless messaging is part of their contract.

## Return objects and API behavior

- Return documented objects explicitly with `return(res)` or a descriptive result name.
- Preserve the `vault_pipe` structure: a list containing `data` and `db_con` with class `vault_pipe`.
- Preserve documented column names, order where contractual, classes, keys, and empty-result behavior.
- Use `return_raw_data` only for data-returning query APIs where raw database data is meaningful.
- When `return_raw_data = FALSE`, preserve the processed/lazy package result promised by that function.
- Do not add `return_raw_data` mechanically to validation, connection, or selection helpers.

## Pipes, tidy data, and data masking

Preserve established `%>%` pipelines where magrittr or dbplyr behavior is already tested. Prefer `|>` for new self-contained code when it does not change lazy-query dispatch or placeholder semantics. Do not mix pipe styles within one coherent pipeline.

Prefer modern explicit operations:

- `dplyr::filter()`, `select()`, `mutate()`, `summarise()`, and joins
- `dplyr::join_by()` for new joins when compatible with supported dependency versions
- `purrr::map()`, `map2()`, `imap()`, or `pmap()` followed by explicit row/column binding
- `stringr::str_glue()` for interpolation and `stringr::str_c()` for concatenation

Use `{{ }}` for bare-column forwarding and `.data[[column_name]]` for names stored as character values. Avoid `eval(parse(...))`, `get()` in data masks, partial matching, and accidental use of global variables.

After grouped summaries, use `.groups = "drop"` or `dplyr::ungroup()` unless grouped output is intentional.

## Lazy query and SQL behavior

`vaultkeepr` should push work to SQLite and retain lazy `vault_pipe` chains until collection is part of the documented API.

- Build from `dplyr::tbl(db_con, "<Table>")`.
- Filter, select, join, and aggregate lazily when dbplyr supports the operation.
- Do not call `dplyr::collect()` merely to inspect intermediate data or use an R-only convenience.
- Inspect generated SQL for substantial query changes.
- Select only needed columns before expensive joins.
- Avoid repeated `DBI::dbListTables()`, `colnames()`, or round trips inside loops when one validated lookup can be reused.
- Check join cardinality and key domains; do not silently multiply rows.
- Preserve behavior for empty selections and zero-row lazy results.

If an operation cannot remain lazy, document where collection occurs, why it is necessary, and the expected data size.

## Connection ownership

Also follow `.ai/database-contract.md`.

- `open_vault()` is the production connection constructor.
- A function receiving `con` must validate it rather than silently opening a replacement.
- Never disconnect a DBI connection supplied by the caller.
- A function that opens a temporary/local connection owns cleanup on success and error.
- Never test against a personal or live VegVault database; use the in-memory fixture.
- Do not return a lazy table backed by a connection that has already been closed.

## Performance

Profile before optimizing. Prioritize SQL pushdown, reduced collection, fewer round trips, narrow column selection, and avoiding repeated schema inspection.

- Do not grow vectors/data frames repeatedly in loops; preallocate or map.
- Avoid rowwise R operations when a vectorized or SQL-translatable operation is clear.
- Cache metadata only within a well-defined call or object; do not introduce hidden global caches.
- Use parallel processing only for independent CPU-heavy work, never for concurrent writes to one SQLite connection.
- Add a benchmark or representative performance test when performance is the stated behavior being changed.

## Reproducibility and validation

- Keep fixtures deterministic and free of licensed/private records.
- Set a seed explicitly when randomness is introduced.
- Do not use environment variables as hidden API controls.
- Run focused tests in a clean session, then `devtools::test()`, then `devtools::check()` for package behavior changes.
- Inspect roxygen-generated diffs after `devtools::document()`.
- Validate query classes and SQL as well as collected values when laziness is part of the contract.
