#' @title Load the VegVault version of a database
#' @description
#' Read the current VegVault version of a database from its table
#' `version_control`: the version of the row with the highest `id`.
#' @param db_con
#' A valid `DBI` connection to a VegVault SQLite database.
#' @return
#' A single character string, the VegVault version of the database, for
#' example `"2.0.0"`.
#' @details
#' The VegVault build writes one row per version, in the order of its
#' configuration, so the row with the highest `id` is the current version.
#' The query orders by `id` explicitly: from VegVault 2.0.0, `version` is
#' unique, and SQLite answers an unordered query from the index of that
#' column, in text order, where `"2.10.0"` comes before `"2.9.0"`.
#'
#' Rows without a version are skipped. A database without the table
#' `version_control`, or without version rows, is read as `"1.0.0"`: the
#' test database of the package has no version row, and extracts made by
#' users may have none.
#' @seealso [validate_vegvault_version()], [open_vault()]
#' @keywords internal
load_vegvault_version <- function(db_con) {
  assertthat_cli(
    exp = inherits(db_con, "DBIConnection") && DBI::dbIsValid(db_con),
    msg = "{.arg db_con} must be a valid database connection"
  )

  flag_version_table <-
    "version_control" %in% DBI::dbListTables(db_con)

  # The test database of the package and extracts made by users may have
  #   no version rows; they are read as VegVault 1.0.0
  if (
    isFALSE(flag_version_table)
  ) {
    return("1.0.0")
  }

  # The id decides, not the order in which SQLite returns the rows: from
  #   VegVault 2.0.0, version is unique, and an unordered query follows the
  #   text order of its index, where "2.10.0" comes before "2.9.0"
  vec_version_latest <-
    dplyr::tbl(
      src = db_con,
      from = "version_control"
    ) |>
    dplyr::filter(
      !is.na(.data[["version"]])
    ) |>
    dplyr::arrange(
      dplyr::desc(.data[["id"]])
    ) |>
    utils::head(n = 1L) |>
    dplyr::pull("version")

  if (
    base::length(vec_version_latest) == 0L
  ) {
    return("1.0.0")
  }

  res <-
    base::as.character(vec_version_latest)

  return(res)
}
