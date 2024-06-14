#' @title Extract data from a plan
#' @description Extract data from a vault connection
#' @param con A vault connection
#' @return A data.frame
#' @export
extract_data <- function(con) {
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

  sel_data <- con$data

  sel_data %>%
    dplyr::collect() %>%
    return()
}
