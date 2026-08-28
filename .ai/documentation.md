# Documentation and pkgdown

## Sources and generated files

- Edit roxygen in `R/*.R`; regenerate `man/` and `NAMESPACE` with `devtools::document()`.
- Edit `README.qmd`; treat `README.md` as generated.
- Edit vignette sources and their helpers; do not hand-edit rendered vignette output.
- Edit `_pkgdown.yml`, source documentation, or templates; treat `docs/` as generated.
- Build pkgdown through `tools/build_pkgdown_site.R` when a documentation or release task requires it.

Keep code examples reproducible, use the disposable example database, and never include private database paths or licensed records.

## Validation

Render only the affected source during normal documentation work and inspect warnings, links, code output, and layout. Rebuild and visually inspect the full pkgdown site for release preparation or site-wide changes. Package checks remain authoritative for examples and vignettes included in the package build.
