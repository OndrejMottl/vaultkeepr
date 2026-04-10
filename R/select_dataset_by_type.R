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

  assertthat_cli(
    is.character(sel_dataset_type),
    msg = "{.arg sel_dataset_type} must be a character vector"
  )

  assertthat_cli(
    all(sel_dataset_type %in% c("vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints")),
    msg = "{.arg sel_dataset_type} must be one of: {.code vegetation_plot}, {.code fossil_pollen_archive}, {.code traits}, {.code gridpoints}"
  )

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
    "dataset_type" %in% colnames(sel_data),
    msg = "The data does not contain the {.code dataset_type} column. Use {.fn get_datasets} before this function"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  vec_present_dataset_type <-
    sel_data %>%
    dplyr::distinct(.data$dataset_type) %>%
    dplyr::collect() %>%
    purrr::pluck(1)

  assertthat_cli(
    any(sel_dataset_type %in% vec_present_dataset_type),
    msg = "The data does not contain any of the dataset types specified in {.arg sel_dataset_type}"
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
