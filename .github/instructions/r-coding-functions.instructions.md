---
applyTo: "**/*.R"
description: >
  Guidelines for writing R functions in the RRatepol package: argument style,
  anonymous functions, error handling with assertthat, roxygen2 documentation,
  and testthat testing.
---

# Function Writing Guidelines

For function calls, always state the arguments even though R can have anonymous
arguments. The only exception is for functions where arguments are not known
(i.e. `...` argument).

## Creating Functions

Specific rules apply for making custom functions:

- For naming of functions see the Naming Conventions section in
  [r-coding.instructions.md](r-coding.instructions.md)
- Each function (declaration) should be placed in a separate script in `R/`,
  named after the function. There should be only a single exported function
  per file (internal helpers may reside in the same file if tightly coupled)
- Function should always return (`return(res_value)`)
- Exported functions must have `@export` in their roxygen2 documentation
- Internal (non-exported) helpers should be prefixed with `.` (e.g.
  `.compute_helper()`) and must **not** have `@export`

**All function creation and editing follows Test-Driven Development (TDD).**
The mandatory cycle is:
1. Write (or update) the roxygen2 spec stub first
2. Write unit tests against the spec — before any implementation exists
3. Verify every test fails against the stub
4. Implement the function until all tests pass
5. Run `devtools::document()` to regenerate `man/*.Rd`
6. Run the full test suite with `devtools::test()`
7. Run `devtools::check()` to confirm the package passes R CMD check

See [make_test_file_for_a_function.instructions.md](make_test_file_for_a_function.instructions.md)
for the full TDD test-writing rules.

## Anonymous Functions

In various instances, it might be better to not create a new function but to
use an anonymous function (e.g. inside of `purrr::map_*()`).

Use tilde (`~`) for anonymous functions in purrr:

```r
purrr::map(
  .f = ~ {
    mean(.x)
  }
)
```

For `purrr::pmap_*()`, use `..1`, `..2`, etc:

```r
purrr::pmap(
  .l = list(
    list_1,
    list_2,
    list_3
  ),
  .f = ~ {
    ..1 + ..2 + ..3
  }
)
```

## Error Handling

Use two different tools depending on what is being checked:

### Argument validation — `assertthat::assert_that()`

Use `assertthat::assert_that()` (from the
[assertthat](https://github.com/hadley/assertthat) package) to validate
function **arguments** (types, required columns, lengths, etc.). These
checks guard against incorrect inputs supplied by the caller:

```r
# Good — argument type and structure checks
assertthat::assert_that(
  base::is.numeric(x),
  msg = "'x' must be numeric."
)

assertthat::assert_that(
  base::all(c("col_a", "col_b") %in% base::names(df)),
  msg = paste0(
    "'df' must contain columns 'col_a' and 'col_b'."
  )
)

# Avoid - plain stop() gives no structured context
if (!is.numeric(x)) stop("x must be numeric")
```

### Internal / data-content checks — `RUtilpol::check_if_integer()`

For internal checks within the function body, prefer the validation helpers
from `RUtilpol` (already a package dependency) where they exist, or use
`assertthat::assert_that()` for custom conditions.

## Function Documentation

Each exported function must have roxygen2 documentation immediately before
the function declaration. Follow the template in
[make_roxygen2_documentation.instructions.md](make_roxygen2_documentation.instructions.md).

Keep the 80-character line limit for all R code and `#'` roxygen2 comment
lines.

```r
#' @title Title of the function
#' @description
#' Description of the function.
#' @param arg1
#' Description of the first argument.
#' @param arg2
#' Description of the second argument.
#' @return
#' Description of the return value.
#' @details
#' Details about the function.
#' @seealso [related_function()]
#' @export
#' @examples
#' # minimal reproducible example using package data
#' data(example_data)
#' result <- my_function(example_data)
```

## The `return_raw_data` Pattern

Functions that query the database and return data to the user must support
a `return_raw_data` argument (default `FALSE`):

- **`return_raw_data = FALSE` (default)** — human-readable output:
  - `sample_id` (and other internal integer IDs) are replaced with
    their name equivalents (`sample_name`, `taxon_name`, etc.) by
    joining with the relevant lookup tables.
  - Where appropriate the result is also **reshaped** (e.g.
    pivoted from long to wide) for easier downstream use.
- **`return_raw_data = TRUE`** — raw database output:
  - Internal IDs (`sample_id`, etc.) are preserved.
  - Long-format structure is kept.
  - This mode is required for join-based analyses that need to link
    different query results at the sample level.

### Rules

1. Always name the argument `return_raw_data` and default it to `FALSE`.
2. Validate with
   `assertthat::assert_that(is.logical(return_raw_data), msg = "…")`
   immediately after the connection/data checks.
3. Use an early-return guard for the raw path:
   ```r
   if (isTRUE(return_raw_data)) {
     res <-
       dplyr::collect(data_res_raw)

     return(res)
   }
   ```
4. Document both output shapes in the `@param return_raw_data` and
   `@return` roxygen2 blocks.
5. Tests must cover:
   - Default output columns and shape.
   - `return_raw_data = TRUE` output columns (raw IDs preserved).
   - Error when `return_raw_data` is not a `logical`.

### Functions that implement this pattern

| Function | Default output | `return_raw_data = TRUE` |
|---|---|---|
| `extract_data()` | packed nested tibble, `sample_name` | flat tibble, `sample_id` |
| `get_age_uncertainty()` | wide tibble, `sample_name` + one col per iteration | long tibble, `sample_id`, `iteration`, `age_uncertainty` |

When adding a new data-returning function, follow the same pattern and
append a row to the table above.

## The `return_raw_data` Pattern

Functions that query the database and return data to the user must support
a `return_raw_data` argument (default `FALSE`):

- **`return_raw_data = FALSE` (default)** — human-readable output:
  - `sample_id` (and other internal integer IDs) are replaced with
    their name equivalents (`sample_name`, `taxon_name`, etc.) by
    joining with the relevant lookup tables.
  - Where appropriate the result is also **reshaped** (e.g. pivoted
    from long to wide) for easier downstream use.
- **`return_raw_data = TRUE`** — raw database output:
  - Internal IDs (`sample_id`, etc.) are preserved.
  - Long-format structure is kept.
  - Required for join-based analyses that need to link different query
    results at the sample level.

### Rules

1. Always name the argument `return_raw_data` and default it to `FALSE`.
2. Validate with
   `assertthat::assert_that(is.logical(return_raw_data), msg = "…")`
   immediately after the connection / data checks.
3. Use an early-return guard for the raw path:
   ```r
   if (isTRUE(return_raw_data)) {
     res <-
       dplyr::collect(data_res_raw)

     return(res)
   }
   ```
4. Document both output shapes in the `@param return_raw_data` and
   `@return` roxygen2 blocks.
5. Tests must cover:
   - Default output columns and shape.
   - `return_raw_data = TRUE` output columns (raw IDs preserved).
   - Error when `return_raw_data` is not a `logical`.

### Functions that implement this pattern

| Function | Default output | `return_raw_data = TRUE` |
|---|---|---|
| `extract_data()` | packed nested tibble, `sample_name` | flat tibble, `sample_id` |
| `get_age_uncertainty()` | wide tibble, `sample_name` + one col per iteration | long tibble, `sample_id`, `iteration`, `age_uncertainty` |

When adding a new data-returning function, follow the same pattern and
append a row to the table above.

## Testing Functions

All tests are done using the [testthat](https://testthat.r-lib.org/) package.
Each function should have its own test file in `tests/testthat/`, named after
the function (e.g., `test-<function_name>.R`). See
[make_test_file_for_a_function.instructions.md](make_test_file_for_a_function.instructions.md)
for the full conventions.

Generally, the function should be tested for:

- output of correct type
- output of correct data
- handling of input errors

**Reproducibility:**
- When randomness is involved, always use `set.seed(900723)` as the standard
  seed value for this project
