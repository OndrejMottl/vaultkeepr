#' @title Extract data from a plan
#' @description Extract data from a vault connection
#' @param con A vault connection
#' @param return_raw_data A logical indicating whether to return raw data or
#' readable column names. Default is `FALSE`.
#' @return A data.frame
#' @export
extract_data <- function(
    con,
    return_raw_data = FALSE) {
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
    inherits(sel_data, "tbl"),
    msg = "data must be a class of `tbl`"
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "path does not lead to valid SQLite database"
  )

  assertthat::assert_that(
    is.logical(return_raw_data),
    msg = "The 'return_raw_data' must be a logical"
  )

  if (
    isFALSE(return_raw_data)
  ) {
    sel_data <-
      get_readable_column_names(
        con = sel_con,
        data = sel_data
      )
  }

  sel_data %>%
    dplyr::collect() %>%
    return()
}
