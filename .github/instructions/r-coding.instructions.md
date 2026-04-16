---
applyTo: "**/*.R"
description: >
  R coding conventions and style guidelines for this project, covering
  naming conventions, syntax rules, function documentation with roxygen2,
  and testing with testthat.
---

# R Coding Conventions and Style Guide

## Coding Style

This coding style is a combination of various sources
([Tidyverse](https://style.tidyverse.org/index.html),
[Google](https://google.github.io/styleguide/Rguide.html), and others).
Style should be consistent within a project.

## Naming Conventions

```r
"There are only two hard things in Computer Science:
 cache invalidation and naming things."
```

### Object Names

Objects and functions should use `snake_style`. The `.` in names is somewhat
popular but it causes issues with names of methods and should be therefore
avoided. The names are preferred to be very descriptive, more expressive and
more explicit.

The names should be nouns and start with the type of object:

- `data_*` - for data
  - special subcategory is `table_*` for tables (mainly as an object for
    reference). Note that all tables can be data but not vice versa.
- `list_` - for lists
- `vec_` - for vectors
- `mod_*` - for statistical model
- `res_` - special category, which can be used within the function to name an
  object to be returned (`return(res_*)`).

Examples of good names:

```r
# data
data_diversity_survey

# list
list_diversity_individual_plots

# vector
vec_region_names

# model
mod_diversity_linear

# result
res_estimated_weight
```

### Function Names

Names of functions should be verbs and describe the expected functionality.

Examples of good function names:

```r
estimate_alpha_diversity()

get_first_value()

transform_into_character()
```

#### Internal Functions

It is possible to start a function with a `"."` (e.g., `.get_round_value()`)
to flag internal functions.

### Column (Variable) Names in Data Frames

`snake_style` is preferred for column names in both `data.frames` and
`tibbles`. Note that the [janitor](https://sfirke.github.io/janitor/) package
can be used to edit this automatically.

## Syntax

Many of the syntax issues can be checked/fixed by
[lintr](https://lintr.r-lib.org/) and
[styler](https://styler.r-lib.org/index.html) packages, which can be used to
automate lots of the tedious aspects.

### Spaces

Space (`" "`) should always be placed:

- after a comma
- before and after infix operators (`==`, `+`, `-`, `<-`, `~`, etc.)

Exceptions:

- No spaces inside or outside parentheses for regular function calls
- Operators with high precedence should not be surrounded by space:
  `:`, `::`, `:::`, `$`, `@`, `[`, `[[`, `^`, unary `-`

### Code Width

No line of **R code** should be longer than 80 characters (including R
comments).

> **Note:** This 80-character limit applies to R source code only.
> Markdown prose — such as text in `.md` files, `.Rmd` or `.qmd` files,
> or any other documentation — is **not** subject to this limit. Let
> markdown text wrap naturally.

### New Lines

Prefer code that is more vertical than horizontal. Therefore, use quite a lot
of new lines.

Usage of a semicolon (`;`) to indicate a new line is not preferred.

A new line should be:

#### 1. After an Object Assignment (`<-`)

Whenever the right-hand side is a **function call**, place a newline after
`<-` and indent the expression by 2 spaces:

```r
data_diversity <-
  read_data(...)

data_coords <-
  tibble::tibble(x = vec_x, y = vec_y)

list_params <-
  base::list(a = 1, b = 2)

res <-
  my_function(
    arg1 = value1,
    arg2 = value2
  )
```

The **only** assignments that may stay on one line are scalar literals and
`NULL` (i.e., when the RHS is a single literal value, not a function call):

```r
vec_center <- 50.0   # OK: numeric literal
name <- "triangle"   # OK: string literal
flag <- NULL         # OK: NULL
count <- 3L          # OK: integer literal
flag <- TRUE         # OK: logical literal
```

The **exception** for the newline rule is function *definitions* — those keep
`<-` on the same line as `function`:

```r
get_data <- function(...) {
  ...
}
```

#### 2. After a Pipe Operator

Prefer the **native pipe `|>`** (R 4.1+). Note that there should be a space
before a pipe.

```r
data_diversity <-
  get_data() |>
  transform_to_percentages()
```

Use the **magrittr pipe `%>%`** when the native pipe cannot be used cleanly:

- Piping into a function's **non-first argument** (`.` placeholder):
  ```r
  data_diversity %>%
    lm(diversity ~ region, data = .)
  ```
- Piping into **curly brackets** `{ }` to suppress the implicit first-argument
  rule:
  ```r
  vec_diversity %>%
    { . + 1 }
  ```
- Piping into `return()` or other special constructs inside a function body
  where `|>` would be ambiguous.

#### 3. After a Function Argument

This should be true for both function declaration and usage. The exception is
a single argument.

```r
get_data <- function(arg1 = foo,
                     arg2 = here::here()) {
  ...
}

data_diversity <-
  get_data(
    arg1 = foo,
    arg2 = here::here()
  )

vec_selected_regions <-
  get_regions(arg1 = foo)
```

#### 4. Parentheses

Each type of parentheses (brackets) has its own rules:

##### Round `( )`

- should not be placed on separate first and last line
- always space *before* the bracket (*unless* it's a function)
- new line after start if it is a multi-argument function

Examples:

```r
1 + (a + b)

get_data(arg = foo)

get_data(
  agr1 = foo,
  agr2 = here::here()
)
```

##### Square `[ ]`

- Never space before the bracket
- always space instead of missing value

Examples:

```r
list_diversity_for_each_plot[[1]]

data_cars[, 2]
```

##### Curly `{ }`

- Use only for functions and expressions
- `{` should be the last character on a line and should never be on its own
- `}` should be the first character on a line
- Always new brackets after else unless followed by if
- Not used for chunks of code

Examples:

```r
get_data <- function(agr1) {
  ...
}

if (
  logical_test
) {
  ...
} else {
  ...
}

try(
  expr = {
    ...
  }
)
```

For `for()`, `if()`, and `while()` loops, the iterator or condition is placed
on its own indented line, and the closing `) {` is on its own line at the same
indent level as the keyword. **This applies even for short, single-condition
tests — never write `if (condition) {` on a single line.**

```r
# Good
for (
  col_name in vec_col_names
) {
  ...
}

if (
  logical_test
) {
  ...
}

while (
  condition
) {
  ...
}

# Avoid
for (i in seq_len(n)) { ... }
if (flag) { ... }
while (condition) { ... }
```

### Assignment

Always use the left assignment `<-`.

Do **NOT** use:

- right assignment (`->`)
- equals (`=`)

There should be a new line after the assignment. Note that rarely single-line
assignment can be used:

```r
data_diversity <-
  get_data()

preferred_shape <- "triangle"
```

### Logical Evaluation

Always use `TRUE` and `FALSE`, instead of `T` and `F`.

## Comments

### Single-line comments

Adding comments to code plays a pivotal role in ensuring reproducibility and
preserving code knowledge for future reference. There's no need to comment
excessively or unnecessarily, but a comment describing what a large or complex
chunk of code does may be helpful. More importantly, it is crucial to comment
WHY something is coded in that specific (non-standard) way. The first letter of
a comment is capitalized and spaced away from the pound sign (`#`).

Example of a single-line comment:

```r
# This is a comment.
```

### Multi-line comment

Multi-line comments should start with a capital letter and the new line should
start with one tab.

Example of a multi-line comment:

```r
# This is a very long comment, where I need to describe
#    what this code is doing
```

### Inline comment

Inline comments should always start with a space.

Example of inline comment:

```r
function(
 agr = 1 # This is an example of an inline comment
)
```

## Section Headers (within long R files)

For longer R files (e.g. complex function implementations), sections can be
marked with headers. Each section should begin with a header which consists of
a name wrapped by two lines, followed by `-----` so that it is automatically
picked up by the IDE as a section header.

Headings can have various hierarchies:

1. `#----------------------------------------------------------#`
2. `#--------------------------------------------------#`
3. `#----------------------------------------#`

Example of a header:

```r
#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
```

## Related Instruction Files

Detailed rules for specific topics are in separate files:

- [r-coding-tidyverse.instructions.md](r-coding-tidyverse.instructions.md) —
  Tidyverse preferences, namespace, modern dplyr/purrr patterns, data masking
- [r-coding-functions.instructions.md](r-coding-functions.instructions.md) —
  Creating functions, anonymous functions, error handling, documentation,
  testing
- [r-coding-performance.instructions.md](r-coding-performance.instructions.md) —
  Profiling, loop performance, parallel processing
