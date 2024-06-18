#' @title Get abiotic data
#' @description Get abiotic data from the Vault database
#' @param con A connection object created by `open_vault()`
#' @return A connection object with abiotic data
#' @export
get_abiotic <- function(con = NULL) {
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

  assertthat::assert_that(
    "sample_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `sample_id` columns. Please add",
      "`get_samples()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "AbioticData" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "AbioticData table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "AbioticData")),
    msg = paste(
      "The AbioticData does not contain `sample_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  data_res <-
    sel_data %>%
    dplyr::left_join(
      dplyr::tbl(sel_con, "AbioticData"),
      by = "sample_id"
    )

  res <-
    structure(
      list(
        data = data_res,
        db_con = sel_con
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
