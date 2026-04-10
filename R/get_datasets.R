#' @title get Datasets
#' @description Get the Datasets tables from the Vault database
#' @param con A connection object created by `open_vault()`
#' @return A `vault_pipe` object with the Datasets table
#' @export
get_datasets <- function(con = NULL) {
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
    inherits(sel_data, "data.frame"),
    msg = "{.code data} must be a class of {.cls data.frame}."
  )

  assertthat_cli(
    nrow(sel_data) == 0,
    msg = "Plan already has some data. {.fn get_datasets} should be selected first in the pipeline after {.fn open_vault}."
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code db_con} must be a class of {.cls SQLiteConnection}."
  )

  assertthat_cli(
    "Datasets" %in% DBI::dbListTables(sel_con),
    msg = "{.code Datasets} table does not exist in the Vault database. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "DatasetTypeID" %in% DBI::dbListTables(sel_con),
    msg = "{.code DatasetTypeID} table does not exist in the Vault database. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "dataset_type_id" %in% colnames(dplyr::tbl(sel_con, "Datasets")),
    msg = "The {.code Datasets} table does not contain {.code dataset_type_id} column. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "dataset_type_id" %in% colnames(dplyr::tbl(sel_con, "DatasetTypeID")),
    msg = "The {.code DatasetTypeID} table does not contain {.code dataset_type_id} column. Make sure to connect to the correct database."
  )

  dat_res <-
    dplyr::left_join(
      dplyr::tbl(sel_con, "Datasets"),
      dplyr::tbl(sel_con, "DatasetTypeID"),
      by = "dataset_type_id"
    )

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
