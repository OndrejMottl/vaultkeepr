#' @title Extract data from a plan
#' @description Extract data from a vault connection
#' @param con A vault connection
#' @param return_raw_data A logical indicating whether to return raw data or
#' without any processing. Default is `FALSE`.
#' @param ... Additional argument passed to `pack_data()`
#' @return A data.frame
#' @export
extract_data <- function(
    con,
    return_raw_data = FALSE,
    verbose = TRUE) {
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

  assertthat::assert_that(
    is.logical(verbose),
    msg = "The 'verbose' must be a logical"
  )

  if (
    isTRUE(return_raw_data)
  ) {
    res <-
      sel_data %>%
      dplyr::collect()

    return(res)
  }

  data_readble <-
    get_readable_column_names(
      con = sel_con,
      data = sel_data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = verbose
    )

  return(data_packed)
}
