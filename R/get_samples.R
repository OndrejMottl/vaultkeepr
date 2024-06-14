#' @title Get samples
#' @description Get samples from the Vault database
#' @param con A connection to the Vault database
#' @return A connection to the Vault database
#' @export
get_samples <- function(con = NULL) {
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
    "dataset_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `dataset_id` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "DatasetSample" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "DatasetSample table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "dataset_id" %in% colnames(dplyr::tbl(sel_con, "DatasetSample")),
    msg = paste(
      "The DatasetSample does not contain `dataset_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "Samples" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "Samples table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "Samples")),
    msg = paste(
      "The Samples does not contain `sample_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  data_res <-
    sel_data %>%
    dplyr::inner_join(
      dplyr::tbl(sel_con, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      dplyr::tbl(sel_con, "Samples"),
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
