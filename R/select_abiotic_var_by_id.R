#' @title Select abiotic variables by id
#' @description Filter the data by abiotic variables id
#' @param con A class of `vault_pipe`
#' @param sel_var_id A character vector of abiotic variable ids
#' @return A class of `vault_pipe`
#' @export
select_abiotic_var_by_id <- function(con = NULL, sel_var_id = NULL) {
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
      "`get_abiotic_data()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "dataset_type" %in% colnames(sel_data),
    msg = paste(
      "The data should be filtered only for `gridpoints`.",
      "However, the does not contain `dataset_type` columns. Please add",
      "`select_dataset_by_type()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    is.numeric(sel_var_id),
    msg = "`sel_var_id` must be a numeric vector"
  )

  assertthat::assert_that(
    all(round(sel_var_id) == sel_var_id),
    msg = "`sel_var_id` must be a integer vector"
  )

  assertthat::assert_that(
    length(sel_var_id) > 0,
    msg = "`sel_var_id` must have at least one taxon id"
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
