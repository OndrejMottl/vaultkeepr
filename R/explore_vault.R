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
  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a {.cls vault_pipe} object. Use {.fn open_vault} to create a connection"
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con} elements. Use {.fn open_vault} to create a connection"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  res <-
    DBI::dbListTables(sel_con)

  return(res)
}
