#' @title get Datasets
#' @description Get the Datasets table from the Vault database
#' @param con A connection object created by `open_vault()`
#' @return A `vault_pipe` object with the Datasets table
#' @export
get_datasets <- function(con = NULL) {
  assertthat::assert_that(
    inherits(con, "vault_pipe"),
    msg = paste(
      "`con` must be a class of `vault_pipe`",
      "Use `open_vault()` to create a connection"
    )
  )

  assertthat::assert_that(
    all(names(con) %in% c("data", "db_con")),
    msg = paste(
      "con must have `data` and `db_con`",
      "Use `open_vault()` to create a connection"
    )
  )

  sel_data <- con$data

  assertthat::assert_that(
    inherits(sel_data, "data.frame"),
    msg = "data must be a class of `data.frame`"
  )

  assertthat::assert_that(
    nrow(sel_data) == 0,
    msg = paste(
      "Plan already has some data.",
      "`get_datasets()` should be selected first in the pipeline",
      "after `open_vault()`"
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "Datasets" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "Datasets table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  dat_res <-
    dplyr::tbl(sel_con, "Datasets")

  res <-
    structure(
      list(
        data = dat_res,
        db_con = sel_con
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
