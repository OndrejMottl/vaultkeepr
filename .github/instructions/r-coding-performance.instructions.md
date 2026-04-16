---
applyTo: "**/*.R"
description: >
  Performance guidelines for R code: profiling before optimising,
  avoiding object growth in loops, and parallel processing guidance.
---

# Performance Guidelines

## Profile Before Optimising

Never attempt to optimise code without first measuring where time is actually
spent. Use `profvis` for interactive profiling and `bench::mark()` for
benchmarking alternatives:

```r
# Identify bottlenecks
profvis::profvis({
  your_analysis(data_full)
})

# Compare alternative implementations
bench::mark(
  approach_a = method_a(data),
  approach_b = method_b(data),
  min_iterations = 10
)
```

## Avoid Growing Objects in Loops

Pre-allocate or use `purrr::map()` instead of growing objects iteratively:

```r
# Avoid - very slow due to repeated copying
vec_result <- c()
for (i in seq_along(vec_ids)) {
  vec_result <- c(vec_result, compute(vec_ids[i]))
}

# Good - pre-allocate
vec_result <- vector("list", length(vec_ids))
for (i in seq_along(vec_ids)) {
  vec_result[[i]] <- compute(vec_ids[i])
}

# Better - use purrr
list_result <-
  purrr::map(
    .x = vec_ids,
    .f = ~ compute(.x)
  )
```

## Parallel Processing

Use parallel processing only for CPU-intensive, independent operations where
the computation cost clearly exceeds the parallelisation overhead. Avoid
parallelising fast or memory-intensive operations.

The RRatepol package uses `pbapply` for parallel iteration with progress bars.
When adding new parallel operations, follow the existing patterns in
`estimate_roc.R` and `run_iteration.R` for consistency.
