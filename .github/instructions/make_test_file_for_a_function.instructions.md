---
applyTo: "**/tests/testthat/*.R"
description: >
  Instructions for writing complete testthat test files for RRatepol package
  functions. Applies when creating or editing test files in tests/testthat/.
---

# Instructions for Writing Complete Test Files for R Functions

## Your Role

You are an expert R developer and testthat user tasked with writing
comprehensive test suites for the RRatepol package.

**IMPORTANT:** All code must follow the project's R coding conventions defined
in `.github/instructions/r-coding.instructions.md`. This includes:

- Object naming conventions (snake_case with type prefixes: `data_*`, `vec_*`,
  `list_*`, `mod_*`)
- Syntax rules (spaces, new lines, assignment with `<-`)
- Function namespace usage (`package::function()`)
- Line width limit (80 characters per line of **R code**)
- Use of `TRUE`/`FALSE` instead of `T`/`F`

### Most-commonly violated rules in test files

These rules are frequently missed — treat them as a checklist before finishing
any test file:

**1. 80-character line limit applies to all R code**, including `test_that()`
description strings. If a description string would exceed 80 characters,
shorten it (do **not** break the string with `paste0()` unless truly
unavoidable). Count the leading spaces + quotes + text + `",` — all of it
counts.

**2. Never use `$` to access data frame columns.** The project standards
explicitly ban `df$column`. Use `dplyr::pull(df, column)` instead:

```r
# Wrong
res$roc_mean

# Correct
dplyr::pull(res, roc_mean)
```

**3. Newline after `<-` when the RHS is a function call.** Every assignment
where the right-hand side is a function call must be split across two lines
(newline + 2-space indent after `<-`):

```r
# Wrong — function call on the same line as <-
data_community <- tibble::tibble(x = 1, y = 2)
res <- my_function(arg = value)

# Correct — newline + 2-space indent after <-
data_community <-
  tibble::tibble(x = 1, y = 2)

res <-
  my_function(arg = value)
```

The **only** assignments that may stay on one line are scalar literals and
`NULL`:

```r
vec_center <- 50.0   # OK: numeric literal
name <- "triangle"   # OK: string literal
flag <- NULL         # OK: NULL
count <- 3L          # OK: integer literal
```

**4. Always use fully-qualified namespaces**, even for base R and testthat
functions.

All `testthat` assertion and block functions must be prefixed with
`testthat::`:

| Bare call | Namespaced form |
|-----------|-----------------|
| `test_that(…)` | `testthat::test_that(…)` |
| `expect_error(…)` | `testthat::expect_error(…)` |
| `expect_equal(…)` | `testthat::expect_equal(…)` |
| `expect_true(…)` | `testthat::expect_true(…)` |
| `expect_false(…)` | `testthat::expect_false(…)` |
| `expect_named(…)` | `testthat::expect_named(…)` |
| `expect_length(…)` | `testthat::expect_length(…)` |
| `expect_warning(…)` | `testthat::expect_warning(…)` |
| `expect_message(…)` | `testthat::expect_message(…)` |
| `expect_s3_class(…)` | `testthat::expect_s3_class(…)` |
| `expect_type(…)` | `testthat::expect_type(…)` |

Common base R calls that must also be namespaced in test files include:

| Bare call | Namespaced form |
|-----------|-----------------|
| `nrow(x)` | `base::nrow(x)` |
| `ncol(x)` | `base::ncol(x)` |
| `colnames(x)` | `base::colnames(x)` |
| `is.data.frame(x)` | `base::is.data.frame(x)` |
| `all(x)` | `base::all(x)` |
| `any(x)` | `base::any(x)` |
| `sort(x)` | `base::sort(x)` |
| `unique(x)` | `base::unique(x)` |
| `length(x)` | `base::length(x)` |
| `seq_along(x)` | `base::seq_along(x)` |
| `seq_len(n)` | `base::seq_len(n)` |
| `paste0(...)` | `base::paste0(...)` |
| `sample(x, n)` | `base::sample(x, n)` |
| `rep(x, n)` | `base::rep(x, n)` |
| `sd(x)` | `stats::sd(x)` — **NOT** `base::sd()` |

## Test File Organization

**Workflow:**

1. **Identify the function** to test in `R/`
2. **Check for an existing test file** in `tests/testthat/`
3. **Create or edit** the test file named `test-<function_name>.R`
4. **When creating a new test file**, insert the function name into the `Config/testthat/start-first` field in `DESCRIPTION` at the earliest position that is still after every function already referenced in its test file's pipe chains.

**File structure:**

- **No script header** — test files must NOT start with a project banner.
  Start the file directly with the first `testthat::test_that()` block or a
  section comment.
- The package is already loaded via `devtools::load_all()` — do NOT call
  `library(RRatepol)` or source any file
- Use `example_data` (the package's built-in dataset) for test data where
  possible: `data("example_data", package = "RRatepol")`
- Use multiple `test_that()` blocks grouped logically
- Name tests descriptively:
  `"function_name() validates input types"`,
  `"function_name() returns correct structure"`

## Core Principle: Test Intended Behavior, Not Implementation Bugs

**CRITICAL:** Tests should capture the *intended* behaviour, not reproduce
potentially incorrect current behaviour.

**Inference hierarchy:**

1. **Primary sources** (highest priority):
   - Function name (assume descriptive and meaningful)
   - Argument names and default values
   - Roxygen2 comments (`#'`)
   - Inline comments
2. **Secondary source** (for technical details only):
   - Actual code implementation

**When there's conflict:** Write tests that enforce the name/comment-based
intention, not the implementation.

**Output format:** Return ONLY valid R code for the test file (no
explanations, no prose, no standalone comments).

---

## What to Test

### 1. Input Validation

**Valid inputs (happy path):**
- At least one valid example where all arguments are correct
- For data.frames/lists: verify correct column/element names are accepted
- For constrained arguments: test valid values work without error

**Invalid inputs (error handling):**
- Wrong class/type for each argument
- Invalid values (e.g. unsupported `method` strings, negative counts)
- Use `testthat::expect_error()` with a regex to match error messages

**Special values:**
- Test `NA`, `NULL`, empty vectors, zero-row data frames where relevant

### 2. Output Structure

Using valid inputs, verify:

- Type and class with `testthat::expect_s3_class()` or `testthat::expect_type()`
- For tibbles/data frames: check `base::nrow()`, `base::ncol()`,
  `base::colnames()`
- For lists: check `base::length()` and element names with
  `testthat::expect_named()`

### 3. Functional Correctness

- Build small, controllable inputs (tiny tibbles or vectors)
- Compute expected results manually or use simple base R
- Compare with `testthat::expect_equal()` (exact) or
  `testthat::expect_equal(..., tolerance = 1e-8)` (floating-point)

### 4. Argument Combinations

- Test varying `method` values
- Toggle logical switches
- Provide vs omit optional arguments

### 5. Randomness

If a function uses randomness, wrap the call with `set.seed(900723)`:

```r
base::set.seed(900723)
res <-
  stochastic_function(data = data_community)
```

Always use `900723` as the standard seed value for this package.

---

## TDD Context: Tests Are Written Before Implementation

This package follows **Test-Driven Development (TDD)**. Test files are
created from the function **spec stub** (roxygen2 docs + empty body) — not
from a finished implementation.

**Tests must describe the intended behaviour.**

### Verification workflow

**Step 1 — Load the package and run the targeted test:**

```r
devtools::load_all()

testthat::test_file(
  "tests/testthat/test-<function_name>.R"
)
```

When testing against a stub, every test must fail. A passing test against an
unimplemented stub is not guarding real behaviour — revise it.

**Step 2 — After full implementation, run the full test suite:**

```r
devtools::test()
```

All tests must pass before considering the task complete.

**Step 3 — Run R CMD check:**

```r
devtools::check()
```

No new errors or warnings should be introduced.

---

## Style and Formatting Rules

- Use `snake_case` for all object names with type prefixes
- Assignment with `<-` (never `=` or `->`)
- Use `TRUE`/`FALSE` (never `T`/`F`)
- Maximum 80 characters per line of R code
- Space after commas, before/after infix operators
- Use full namespace: `package::function()` for all function calls
- Vertical code style — newline after `<-` when RHS is a function call

**No extra output:**
- Do NOT print anything or include prose
- Return ONLY valid R code for the test file
