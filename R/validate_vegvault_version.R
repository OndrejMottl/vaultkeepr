#' @title Validate the VegVault version of a database
#' @description
#' Stop with an error when vaultkeepr does not support the VegVault version
#' of a database.
#' @param database_version
#' A single character string, the VegVault version of the database, as
#' read by [load_vegvault_version()].
#' @param verbose
#' Logical. If `TRUE` (default), the reason for an error is printed to the
#' console via `cli`. The error itself occurs either way.
#' @return
#' `database_version`, invisibly, when vaultkeepr supports it.
#' @details
#' vaultkeepr supports the major versions 1 and 2 of VegVault (so far, the
#' releases 1.0.0 and 2.0.0). VegVault 1.0.0 is the first release of the
#' database, and a new major version may change the tables that vaultkeepr
#' reads.
#' The function stops for an older or a newer version, and for a version
#' that cannot be read as a version number, such as `"2.0.0-dev"`.
#' @seealso [load_vegvault_version()], [open_vault()]
#' @keywords internal
validate_vegvault_version <- function(
  database_version,
  verbose = TRUE
) {
  assertthat_cli(
    exp = isTRUE(verbose) || isFALSE(verbose),
    msg = "{.arg verbose} must be TRUE or FALSE"
  )

  assertthat_cli(
    exp = is.character(database_version) &&
      length(database_version) == 1L &&
      !is.na(database_version),
    msg = "{.arg database_version} must be a single version string",
    verbose = verbose
  )

  version_first_supported <- "1.0.0"
  version_first_unsupported <- "3.0.0"

  text_supported <-
    stringr::str_c(
      "VegVault ", version_first_supported,
      " up to, but not including, ", version_first_unsupported
    )

  # Braces are doubled, so that cli prints them instead of evaluating them
  version_label <-
    stringr::str_replace_all(
      string = database_version,
      pattern = c("\\{" = "{{", "\\}" = "}}")
    )

  version_parsed <-
    base::numeric_version(
      x = database_version,
      strict = FALSE
    )

  assertthat_cli(
    exp = !is.na(version_parsed),
    msg = stringr::str_c(
      "The VegVault version {.val ", version_label, "} of the database ",
      "cannot be read as a version number. vaultkeepr supports ",
      text_supported
    ),
    verbose = verbose
  )

  assertthat_cli(
    exp = version_parsed >= version_first_supported,
    msg = stringr::str_c(
      "VegVault {.val ", version_label, "} is older than ",
      version_first_supported, ", the first release of the database. ",
      "vaultkeepr supports ", text_supported
    ),
    verbose = verbose
  )

  assertthat_cli(
    exp = version_parsed < version_first_unsupported,
    msg = stringr::str_c(
      "VegVault {.val ", version_label, "} is newer than this version of ",
      "vaultkeepr supports (", text_supported, "). ",
      "Update vaultkeepr to read it"
    ),
    verbose = verbose
  )

  res <- database_version

  return(base::invisible(res))
}
