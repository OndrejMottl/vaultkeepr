#' @title Explore a vault
#' @description
#' Get the names of all tables in the connected VegVault database.
#' This function can be called at any stage of the pipeline.
#' @param con
#' A `vault_pipe` object created by `open_vault()`.
#' @return
#' A `character` vector of table names present in the VegVault database.
#' @export
explore_vault <- function(con = NULL) {
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

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  res <-
    DBI::dbListTables(sel_con)

  return(res)
}
