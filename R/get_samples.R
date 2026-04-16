#' @title Get samples
#' @description Get samples from the Vault database
#' @param con A connection to the Vault database
#' @return A connection to the Vault database
#' @export
get_samples <- function(con = NULL) {
  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a class of {.cls vault_pipe}. Use {.fn open_vault} to create a connection."
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con}. Use {.fn open_vault} to create a connection."
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code data} must be a class of {.cls tbl}."
  )

  assertthat_cli(
    "dataset_id" %in% colnames(sel_data),
    msg = "The dataset does not contain {.code dataset_id} column. Please add {.fn get_datasets} to the pipe before this function."
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code db_con} must be a class of {.cls SQLiteConnection}."
  )

  assertthat_cli(
    "DatasetSample" %in% DBI::dbListTables(sel_con),
    msg = "{.code DatasetSample} table does not exist in the Vault database. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "dataset_id" %in% colnames(dplyr::tbl(sel_con, "DatasetSample")),
    msg = "The {.code DatasetSample} table does not contain {.code dataset_id} column. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "Samples" %in% DBI::dbListTables(sel_con),
    msg = "{.code Samples} table does not exist in the Vault database. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "Samples")),
    msg = "The {.code Samples} table does not contain {.code sample_id} column. Make sure to connect to the correct database."
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
