#' @title Open a vault
#' @description Open a connection to a SQLite database
#' @param path A character string of the path to the SQLite database
#' @param verbose A logical value indicating whether to print messages
#' @return A list with two elements: `data` and `db_con`
#' @export
open_vault <- function(path, vebose = TRUE) {
  # test various things

  assertthat_cli(
    is.logical(vebose),
    msg = "{.arg vebose} must be a logical"
  )

  assertthat_cli(
    is.character(path),
    msg = "{.arg  path} must be a character",
    verbose = vebose
  )

  assertthat_cli(
    length(path) == 1,
    msg = "{.args path} must be a single string",
    verbose = vebose
  )

  assertthat_cli(
    file.exists(path),
    msg = "file does not exist",
    verbose = vebose
  )


  db_con <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      path
    )

  dummy <-
    structure(
      list(
        data = tibble::tibble(),
        db_con = db_con
      ),
      class = c("list", "vault_pipe")
    )

  if (
    isTRUE(vebose)
  ) {
    cli::cli_alert_info("Vault opened successfully")
  }

  return(dummy)
}
