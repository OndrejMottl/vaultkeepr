---
applyTo: "**/*.R, **/*.Rmd, **/*.qmd"
description: >
  Visualisation conventions for the RRatepol package: ggplot2 theming,
  ggview for interactive sizing, and saving plots in vignettes and the README.
---

# Visualisation Guidelines

## Base Theme

Always use `ggplot2::theme_minimal()` as the base theme for all plots.
Use `ggplot2::labs()` for all axis labels and titles — never `ggtitle()`.

```r
plot_roc <-
  ggplot2::ggplot(data_roc, ggplot2::aes(x = roc, y = age)) +
  ggplot2::geom_line() +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    x = "Rate of change score",
    y = "Age (cal yr BP)"
  )
```

## Canvas Dimensions (vignettes and README)

When producing plots for vignettes or the README that need to be saved at
specific dimensions, append `ggview::canvas()` to the plot pipeline:

```r
plot_example <-
  make_plot(data = data_example) +
  ggview::canvas(
    width = 1600,
    height = 800,
    units = "px"
  )
```

Standard canvas dimensions:
- Full-width landscape: `width = 1600, height = 800`
- Square inset: `width = 800, height = 800`

## Saving Plots

Use `ggview::save_ggplot()` (not `ggplot2::ggsave()`) to save plots built
with `ggview::canvas()`, so that the canvas dimensions are respected:

```r
ggview::save_ggplot(
  plot = plot_example,
  file = here::here("man", "figures", "plot_example.png")
)
```

For plots in vignettes that are rendered inline by knitr, no explicit save
call is needed — just print or return the ggplot object in the chunk.

## Namespace

Always use fully-qualified namespaces for all ggplot2 calls:

```r
# Good
ggplot2::ggplot(...) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal()

# Avoid
ggplot(...) +
  geom_point() +
  theme_minimal()
```
