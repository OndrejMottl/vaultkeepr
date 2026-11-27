#' @title Open a vault
#' @description Open a connection to a SQLite database
#' @param path A character string of the path to the SQLite database
#' @param verbose A logical value indicating whether to print messages
#' @return A `vault_pipe` list with two elements, `data` and `db_con`, and a
#'   `database_version` attribute.
#' @details
#' `open_vault()` reads the VegVault version of the database from the row
#' of `version_control` with the highest `id` and stores it as the
#' `database_version` attribute. vaultkeepr supports VegVault 1 and 2 (so
#' far, the releases 1.0.0 and 2.0.0). For an older or a newer version, or
#' one that cannot be read as a version number, `open_vault()` closes the
#' connection and stops with an error. A database without version rows is
#' read as VegVault 1.0.0.
#'
#' The database is opened read-only: vaultkeepr only reads from it, and a
#' read-only connection guarantees that no query, in vaultkeepr or in user
#' code, changes the file. A statement that writes to the database, such as
#' `CREATE TABLE`, fails with an error. Temporary tables, for example from
#' `dplyr::compute()`, still work, because SQLite keeps them outside the file.
#' @export
open_vault <- function(path, verbose = TRUE) {
  # test various things

  assertthat_cli(
    is.logical(verbose),
    msg = "{.arg verbose} must be a logical"
  )

  assertthat_cli(
    is.character(path),
    msg = "{.arg path} must be a character",
    verbose = verbose
  )

  assertthat_cli(
    length(path) == 1,
    msg = "{.arg path} must be a single string",
    verbose = verbose
  )

  assertthat_cli(
    file.exists(path),
    msg = "file does not exist",
    verbose = verbose
  )

  vec_header <-
    base::readBin(path, what = "raw", n = 15L)

  assertthat_cli(
    base::length(vec_header) == 15L &&
      base::rawToChar(vec_header) == "SQLite format 3",
    msg = "{.arg path} must point to a valid SQLite file",
    verbose = verbose
  )

  # vaultkeepr only reads from the database, so it opens it read-only
  db_con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path,
      flags = RSQLite::SQLITE_RO
    )

  # open_vault() owns the connection until it returns the pipe, so it
  #   closes the connection when the version of the database is not
  #   supported
  database_version <-
    base::withCallingHandlers(
      expr = load_vegvault_version(db_con = db_con) |>
        validate_vegvault_version(verbose = verbose),
      error = function(cnd) {
        DBI::dbDisconnect(conn = db_con)
      }
    )

  dummy <-
    structure(
      list(
        data = tibble::tibble(),
        db_con = db_con
      ),
      class = c("list", "vault_pipe"),
      database_version = database_version
    )

  if (
    isTRUE(verbose)
  ) {
    cli::cli_alert_info("Vault opened successfully")
  }

  return(dummy)
}
