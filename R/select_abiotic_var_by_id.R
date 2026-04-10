#' @title Select abiotic variables by id
#' @description Filter the data by abiotic variables id
#' @param con A class of `vault_pipe`
#' @param sel_var_id A character vector of abiotic variable ids
#' @return A class of `vault_pipe`
#' @export
select_abiotic_var_by_id <- function(con = NULL, sel_var_id = NULL) {
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
    msg = "The data does not contain the {.code dataset_type} column. Use {.fn select_dataset_by_type} before this function"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  assertthat_cli(
    is.numeric(sel_var_id),
    msg = "{.arg sel_var_id} must be a numeric vector"
  )

  assertthat_cli(
    all(round(sel_var_id) == sel_var_id),
    msg = "{.arg sel_var_id} must be an integer vector"
  )

  assertthat_cli(
    length(sel_var_id) > 0,
    msg = "{.arg sel_var_id} must have at least one variable id"
  )

  dat_res <-
    sel_data %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        .default = TRUE,
        !(.data$abiotic_variable_id %in% sel_var_id) &
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
