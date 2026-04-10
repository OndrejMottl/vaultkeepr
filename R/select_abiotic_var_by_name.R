#' @title Select abiotic variables by name
#' @description Filter the data by abiotic variables name
#' @param con A class of `vault_pipe`
#' @param sel_var_name A character vector of abiotic variable names
#' @return A class of `vault_pipe`
#' @export
select_abiotic_var_by_name <- function(con = NULL, sel_var_name = NULL) {
  .data <- rlang::.data

  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a {.cls vault_pipe} object. Use {.fn open_vault} to create a connection"
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con} elements. Use {.fn open_vault} to create a connection"
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code con$data} must be a {.cls tbl}"
  )

  assertthat_cli(
    "abiotic_variable_id" %in% colnames(sel_data),
    msg = "The data does not contain the {.code abiotic_variable_id} column. Use {.fn get_abiotic_data} before this function"
  )

  assertthat_cli(
    "dataset_type" %in% colnames(sel_data),
    msg = "The data does not contain the {.code dataset_type} column. Use {.fn get_datasets} before this function"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  assertthat_cli(
    "AbioticVariable" %in% DBI::dbListTables(sel_con),
    msg = "The {.code AbioticVariable} table does not exist in the database. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "abiotic_variable_id" %in% colnames(dplyr::tbl(sel_con, "AbioticVariable")),
    msg = "The {.code AbioticVariable} table does not contain the {.code abiotic_variable_id} column. Make sure to connect to the correct database"
  )

  assertthat_cli(
    is.character(sel_var_name),
    msg = "{.arg sel_var_name} must be a character vector"
  )

  dat_res <-
    sel_data %>%
    dplyr::left_join(
      dplyr::tbl(sel_con, "AbioticVariable"),
      by = "abiotic_variable_id"
    ) %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        .default = TRUE,
        !(.data$abiotic_variable_name %in% sel_var_name) &
          .data$dataset_type == "gridpoints" ~ FALSE
      )
    ) %>%
    dplyr::filter(
      .data$keep == TRUE
    ) %>%
    dplyr::select(-"keep")

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
