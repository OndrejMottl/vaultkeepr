---
applyTo: "**/R/*.R"
description: >
  Instructions for documenting exported functions in the RRatepol package
  using roxygen2. Applies when creating or editing function documentation
  in R/*.R files.
---

# Instructions for Documenting Functions with {roxygen2}

## The Goal

All exported functions in `R/` must be documented using the
[roxygen2](https://roxygen2.r-lib.org/) package. Documentation comments are
placed immediately before the function declaration and are used to generate
`man/*.Rd` files via `devtools::document()`.

**IMPORTANT:** All code and documentation must follow the project's R coding
conventions defined in `.github/instructions/r-coding.instructions.md`,
including:

- 80 character line limit for all **R code lines** (including `#'` roxygen2
  comment lines inside `.R` files — but **not** for markdown prose in `.md`
  or `.Rmd` files)
- Function naming conventions (verbs, snake_case)
- Proper spacing and formatting

## Regenerating Documentation

After adding or editing roxygen2 comments, regenerate the `man/` folder:

```r
devtools::document()
```

This updates `man/*.Rd` and `NAMESPACE`. Never edit `man/*.Rd` or `NAMESPACE`
by hand — they are auto-generated.

## Documentation Template

```r
#' @title Title of the function
#' @description
#' Description of the function. Keep each line within 80 characters.
#' @param arg1
#' Description of the first argument. State accepted types, valid values,
#' and default behaviour.
#' @param arg2
#' Description of the second argument.
#' @return
#' Description of the return value. State the type (e.g. a `tibble`) and
#' the columns or structure of the returned object.
#' @details
#' Optional extended details about the function's algorithm or behaviour.
#' @seealso [related_function()], [other_function()]
#' @export
#' @examples
#' # Load the built-in example data
#' data("example_data", package = "RRatepol")
#'
#' # Minimal reproducible example
#' result <-
#'   my_function(
#'     data_community = example_data$pollen_data[[1]],
#'     data_age_sequence = example_data$sample_ages[[1]]
#'   )
```

## Rules

### `@title`
- One short sentence, no period at the end
- Capitalise the first word only (sentence case)

### `@description`
- One or two sentences describing what the function does
- Wrap at 80 characters

### `@param`
- One `@param` block per argument
- Start with the argument name on the `@param` line
- Description on the following line(s), indented with the `#'` prefix
- State: type, valid range/choices, and what the default means
- Use backtick-wrapped type names: `` `numeric` ``, `` `character` ``,
  `` `tibble` ``, `` `NULL` ``

### `@return`
- Always present for exported functions
- Describe what is returned, including type and structure

### `@export`
- Include `@export` for all functions that should be part of the public API
- Internal helpers (prefixed with `.`) must **not** have `@export`

### `@examples`
- Runnable examples using `example_data` (the package's built-in dataset)
- Keep examples minimal and fast (< 5 seconds)
- Wrap long-running examples in `\donttest{ }`, never `\dontrun{ }`

### Line Length
- The 80-character limit applies to every `#'` line in the R source file
- This includes the `@title`, `@description`, `@param`, and `@return` lines
- Long URLs in `@seealso` are the only acceptable exception
