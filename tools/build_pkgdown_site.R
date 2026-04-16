build_pkgdown_site <- function() {
  cnd_build_error <- NULL

  # Install the current development version into the user library so that
  # quarto's R subprocess (which doesn't inherit in-session libpaths) can
  # find any newly added or modified functions.
  devtools::install(quiet = TRUE, dependencies = FALSE)

  # Work around a Quarto 1.8.x + Windows bug inside pkgdown:::quarto_render:
  # pkgdown passes --output-dir as an absolute Windows path (e.g.
  # C:\Users\...\pkgdown-quarto-xxx) to the Quarto CLI. Quarto's Deno engine
  # on Windows treats this as a path RELATIVE to the project directory,
  # joining them: <project>\vignettes\C:\Users\...\pkgdown-quarto-xxx.
  #
  # Fix: replace the system-temp output directory with one that lives inside
  # the package source tree (guaranteed to be on the same drive), then pass
  # it to Quarto as a RELATIVE path from the vignettes directory. Quarto
  # resolves relative --output-dir values from the render target correctly.
  #
  # new_process MUST be FALSE so this in-session patch remains active during
  # the build; a callr subprocess would not inherit it.
  local({
    pkg_ns <- getNamespace("pkgdown")
    orig_fn <- pkg_ns[["quarto_render"]]

    patched_fn <- function(pkg, path, quiet) {
      metadata_path <- tempfile(
        pattern = "pkgdown-quarto-metadata-",
        fileext = ".yml"
      )
      on.exit(unlink(metadata_path), add = TRUE)

      write_yaml(quarto_format(pkg), metadata_path)

      proj_build_dir <-
        file.path(pkg$src_path, ".pkgdown-build")
      dir.create(
        proj_build_dir,
        showWarnings = FALSE,
        recursive = TRUE
      )

      output_dir <- tempfile(
        pattern = "pkgdown-quarto-",
        tmpdir = proj_build_dir
      )
      dir.create(output_dir, recursive = TRUE)

      # Relative path from `path` (vignettes/) to output_dir — both are on
      # the same drive so fs::path_rel always succeeds on Windows.
      rel_output_dir <- fs::path_rel(output_dir, path)

      quarto::quarto_render(
        path,
        metadata_file = metadata_path,
        quarto_args = c("--output-dir", rel_output_dir),
        quiet = quiet,
        as_job = FALSE
      )
      output_dir
    }

    environment(patched_fn) <- environment(orig_fn)
    assignInNamespace("quarto_render", patched_fn, ns = "pkgdown")
  })

  on.exit(
    unlink(".pkgdown-build", recursive = TRUE, force = TRUE),
    add = TRUE
  )

  # Patch README.md: Quarto renders fig.alt as data-fig-alt, but pkgdown's
  # accessibility checker requires the standard HTML alt attribute.
  #
  # On Windows, all text-based read/write functions convert through the native
  # locale (CP1252), corrupting multi-byte UTF-8 sequences like U+2139 (ℹ).
  # The only safe approach is to work at the raw byte level throughout:
  #   1. Open a binary connection — avoids file.size() / file.info() which can
  #      return NA under NTFS ACL issues on some Windows accounts.
  #   2. readBin() reads raw bytes with zero encoding conversion.
  #   3. rawToChar() + Encoding() <- "UTF-8" marks the bytes as UTF-8 without
  #      re-encoding them.
  #   4. The gsub() pattern is pure ASCII so works correctly regardless.
  #   5. iconv(..., toRaw = TRUE) converts back to raw UTF-8 bytes, bypassing
  #      the locale entirely.
  #   6. writeBin() writes those raw bytes straight to disk.
  readme_read_con <- file("README.md", open = "rb")
  raw_readme <- readBin(readme_read_con, what = "raw", n = 1e7)
  close(readme_read_con)
  str_readme <- rawToChar(raw_readme)
  Encoding(str_readme) <- "UTF-8"
  str_readme <- gsub(
    pattern = " data-fig-alt=\"",
    replacement = " alt=\"",
    x = str_readme,
    fixed = TRUE
  )
  writeBin(
    object = iconv(
      str_readme,
      from = "UTF-8",
      to = "UTF-8",
      toRaw = TRUE
    )[[1]],
    con = "README.md"
  )

  tryCatch(
    expr = {
      pkgdown::build_site_github_pages(
        pkg = ".",
        install = TRUE,
        new_process = FALSE
      )
    },
    error = function(cnd_error) {
      cnd_build_error <<- cnd_error
    }
  )

  vec_keep_github_pages <-
    c("404", "CODE_OF_CONDUCT", "CONTRIBUTING", "SUPPORT")

  vec_github_md <-
    list.files(
      path = ".github",
      pattern = "\\.md$",
      full.names = FALSE
    )

  vec_drop_pages <-
    setdiff(
      tools::file_path_sans_ext(vec_github_md),
      vec_keep_github_pages
    )

  if (length(vec_drop_pages) > 0L) {
    unlink(
      file.path("docs", paste0(vec_drop_pages, ".html")),
      force = TRUE
    )
  }

  vec_markdown_mirrors <-
    list.files(
      path = "docs",
      pattern = "\\.md$",
      recursive = TRUE,
      full.names = TRUE
    )

  if (length(vec_markdown_mirrors) > 0L) {
    unlink(vec_markdown_mirrors, force = TRUE)
  }

  unlink(file.path("docs", "llms.txt"), force = TRUE)

  if (is.null(cnd_build_error)) {
    getFromNamespace("build_sitemap", ns = "pkgdown")()
    pkgdown::build_search()
  }

  if (!is.null(cnd_build_error)) {
    stop(cnd_build_error)
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) {
  build_pkgdown_site()
}
