#' @title Select abiotic variables by name
#' @description Filter the data by abiotic variables name
#' @param con A class of `vault_pipe`
#' @param sel_var_name A character vector of abiotic variable names
#' @return A class of `vault_pipe`
#' @export
select_abiotic_var_by_name <- function(con = NULL, sel_var_name = NULL) {
  .data <- rlang::.data

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
    "abiotic_variable_id" %in% colnames(sel_data),
    msg = paste(
      "The data does not contain `abiotic_variable_id` columns. Please add",
      "`get_abiotic()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "dataset_type" %in% colnames(sel_data),
    msg = paste(
      "The data should be filtered only for `gridpoints`.",
      "However, the does not contain `dataset_type` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "AbioticVariable" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "AbioticVariable table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "abiotic_variable_id" %in% colnames(dplyr::tbl(sel_con, "AbioticVariable")),
    msg = paste(
      "The AbioticVariable does not contain `abiotic_variable_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    is.character(sel_var_name),
    msg = "`sel_var_name` must be a character vector"
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
