#' @title Check whether a database is VegVault 2 or newer
#' @description
#' Read the VegVault version of a database and test whether it is 2.0.0 or
#' newer, the versions whose trait values carry unit and provenance fields.
#' @param db_con
#' A valid `DBI` connection to a VegVault SQLite database.
#' @return
#' A single logical value: `TRUE` for VegVault 2.0.0 or newer, `FALSE`
#' otherwise.
#' @details
#' The version is read by [load_vegvault_version()], so a database without
#' version rows counts as VegVault 1.0.0. A version that cannot be read as a
#' version number, such as `"2.0.0-dev"`, gives `FALSE`; [open_vault()]
#' rejects such databases before any query runs.
#' @seealso [load_vegvault_version()], [get_trait_values_by_version()]
#' @keywords internal
is_vegvault_v2 <- function(db_con) {
  database_version <-
    load_vegvault_version(db_con = db_con)

  # A version that cannot be read as a version number is not VegVault 2;
  #   open_vault() rejects such databases before any query runs
  res <-
    tryCatch(
      expr = base::numeric_version(x = database_version) >=
        base::numeric_version(x = "2.0.0"),
      error = function(error) FALSE
    )

  return(res)
}
