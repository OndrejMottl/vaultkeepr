#' @title get Datasets by selected type
#' @description Filter the Datasets by a selected DatasetType
#' @param con A connection object created by `open_vault()`
#' @param sel_dataset_type A character vector of dataset types to filter
#' @return A `vault_pipe` object with the filtered Datasets table
#' @export
select_dataset_by_type <- function(
    con = NULL,
    sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints")) {
  .data <- rlang::.data

  assertthat::assert_that(
    is.character(sel_dataset_type),
    msg = "`sel_dataset_type` must be a character vector"
  )

  assertthat::assert_that(
    all(sel_dataset_type %in% c("vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints")),
    msg = paste(
      "The `sel_dataset_type` must be one of the following:",
      "`vegetation_plot`, `fossil_pollen_archive`, `traits`, `gridpoints`"
    )
  )

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
    "dataset_type" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `dataset_type` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  vec_present_dataset_type <-
    sel_data %>%
    dplyr::distinct(.data$dataset_type) %>%
    dplyr::collect() %>%
    purrr::pluck(1)

  assertthat::assert_that(
    any(sel_dataset_type %in% vec_present_dataset_type),
    msg = paste(
      "The data does not contain the any of dataset types specified in",
      "`sel_dataset_type`."
    )
  )

  dat_res <-
    sel_data %>%
    dplyr::filter(
      .data$dataset_type %in% sel_dataset_type
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
