#' @title Open a vault
#' @description Open a connection to a SQLite database
#' @param path A character string of the path to the SQLite database
#' @return A list with two elements: `data` and `db_con`
#' @export
open_vault <- function(path) {
  # test various things
  assertthat::assert_that(
    is.character(path),
    msg = "path must be a character"
  )

  assertthat::assert_that(
    length(path) == 1,
    msg = "path must be a single string"
  )

  assertthat::assert_that(
    file.exists(path),
    msg = "file does not exist"
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

  return(dummy)
}
