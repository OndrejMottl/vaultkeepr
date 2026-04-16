---
applyTo: "**/*.R"
description: >
  Tidyverse usage guidelines: preferred functions over base R, modern dplyr
  and purrr patterns, full namespace rules, and data masking conventions.
---

# Tidyverse & Namespace Guidelines

## Prefer Tidyverse Over Base R

Use tidyverse functions over base R equivalents:

| Base R | Better Style, Performance, and Utility |
|--------|----------------------------------------|
| `read.csv()` | `readr::read_csv()` |
| `df$some_column` | `df |> dplyr::pull(some_column)` |
| `df$some_column = ...` | `df |> dplyr::mutate(some_column = ...)` |
| `list$element` | `list \|> purrr::chuck("element")` |
| `apply(m, 2, f)` | `purrr::map_dbl(colnames(m), ~ f(m[, .x]))` |
| `lapply(x, f)` | `purrr::map(x, f)` |
| `sapply(x, f)` | `purrr::map_dbl(x, f)` / `purrr::map_chr(x, f)` |
| `vapply(x, f, numeric(1))` | `purrr::map_dbl(x, f)` |
| `mapply(f, x, y)` | `purrr::map2(x, y, f)` |
| `grepl("p", x)` | `stringr::str_detect(x, "p")` |
| `gsub("a", "b", x)` | `stringr::str_replace_all(x, "a", "b")` |

**Never use `$` for element access.** Use `dplyr::pull()` to extract a column
from a data frame and `purrr::chuck()` to extract an element from a list.
These are explicit, pipe-friendly, and raise a clear error when the element
is missing.

```r
# Good — data frame column
data_diversity |>
  dplyr::pull(species_richness)

# Good — list element
list_params |>
  purrr::chuck("n_iter")

# Avoid
data_diversity$species_richness
list_params$n_iter
```

**Never use the `apply` family** (`apply()`, `lapply()`, `sapply()`,
`vapply()`, `mapply()`, `tapply()`). Use `purrr::map*()` equivalents
instead — they are type-stable, pipe-friendly, and consistent with the
rest of the tidyverse. This rule applies to all iteration and
functional-programming patterns, not just data-frame operations.

## Namespace

Always use the full package namespace with a function call. This helps to
track the source of a function in a script:

```r
data_diversity |>
  dplyr::mutate(
    beta_diversity = 0
  )
```

## Modern dplyr Patterns

### Joins

Use `join_by()` instead of character vectors for joins (dplyr 1.1+):

```r
# Good
transactions |>
  dplyr::inner_join(companies, by = dplyr::join_by(company == id))

# Avoid
transactions |>
  dplyr::inner_join(companies, by = c("company" = "id"))
```

Use `multiple` and `unmatched` arguments in joins for data quality control:

```r
# Error on unexpected multiple matches
dplyr::inner_join(x, y, by = dplyr::join_by(id), multiple = "error")

# Error if any rows are unmatched
dplyr::inner_join(x, y, by = dplyr::join_by(id), unmatched = "error")
```

### Grouping

Use `dplyr::group_by()` for grouping operations. Always pair it with
`dplyr::ungroup()` after the grouped computation to avoid unexpected behaviour
in downstream steps:

```r
data_diversity |>
  dplyr::group_by(region) |>
  dplyr::summarise(mean_diversity = mean(diversity)) |>
  dplyr::ungroup()
```

Use `reframe()` for summaries that return more than one row per group:

```r
data_diversity |>
  dplyr::group_by(region) |>
  dplyr::reframe(
    quantiles = quantile(diversity, c(0.25, 0.5, 0.75))
  )
```

## Modern purrr Patterns

Use `map() |> list_rbind()` instead of the superseded `map_dfr()`
(purrr 1.0+):

```r
# Good
list_results |>
  purrr::map(~ fit_model(.x)) |>
  purrr::list_rbind()

# Avoid (superseded)
list_results |>
  purrr::map_dfr(~ fit_model(.x))
```

Use `walk()` and `walk2()` for side effects (file writing, plotting) instead
of `map()` or `for` loops when the return value is not needed:

```r
# Good
purrr::walk2(
  list_data,
  vec_file_paths,
  ~ readr::write_csv(.x, .y)
)
```

### Piping the input vector into `purrr::map()`

Prefer piping the input vector directly into `purrr::map()` over using the
`.x` argument. This keeps the iteration subject at the top of the pipe chain
and reads more naturally:

```r
# Good — input is the subject of the pipe
vec_ids |>
  rlang::set_names() |>
  purrr::map(
    .f = ~ compute(.x)
  )

# Avoid — .x = buries the input inside the call
purrr::map(
  .x = vec_ids,
  .f = ~ compute(.x)
)
```

Use `rlang::set_names()` (no argument) before `purrr::map()` to propagate
vector names to the output list.

### Nested `purrr::map()` and `.x` disambiguation

When `purrr::map()` calls are nested, both lambdas share the `.x` pronoun,
causing ambiguity. Resolve this by binding the outer `.x` to a named
variable at the top of the outer function body:

```r
# Good — outer .x is captured as a named variable
vec_ages |>
  rlang::set_names() |>
  purrr::map(
    .f = ~ {
      age_i <- .x   # capture before .x is shadowed

      vec_vars |>
        rlang::set_names() |>
        purrr::map(
          .f = ~ compute(var = .x, age = age_i)
        )
    }
  )

# Avoid — inner .x silently shadows outer .x
purrr::map(
  .x = vec_ages,
  .f = function(age_i) {
    purrr::map(
      .x = vec_vars,
      .f = ~ compute(var = .x, age = age_i)
    )
  }
)
```

## Data Masking

When writing functions that pass column names to tidyverse data-masking
functions (`dplyr::filter()`, `dplyr::mutate()`, `dplyr::summarise()`, etc.),
use the correct forwarding mechanism to avoid ambiguity and masking bugs.

### Forwarding function arguments with `{{ }}`

Use the embrace operator `{{ }}` to forward a function argument into a
data-masking context:

```r
# Good
calculate_group_mean <- function(data, group_var, value_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      mean_value = mean({{ value_var }})
    ) |>
    dplyr::ungroup()
}
```

### Accessing columns by name string with `.data[[]]`

When a column name is provided as a character string (e.g. in a loop), use
`.data[[var]]` instead of `!!sym(var)` for clarity and safety:

```r
# Good - character vector column access
for (col_name in vec_col_names) {
  data_diversity |>
    dplyr::summarise(
      mean_val = mean(.data[[col_name]])
    )
}
```

### Forwarding `...`

When forwarding `...` to data-masking functions, no special syntax is needed:

```r
group_and_summarise <- function(.data, ...) {
  .data |> dplyr::group_by(...)
}
```

### Avoid dangerous patterns

```r
# Avoid - eval/parse is dangerous and fragile
eval(parse(text = paste("mean(", var, ")")))

# Avoid - get() causes name collisions in data masks
with(data, mean(get(var)))
```
