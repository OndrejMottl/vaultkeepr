#' @title Get common dataset type
#' @description Get the common dataset type between the data and
#' the selected dataset type
#' @param data_source A tbl object containing the dataset
#' @param vec_dataset_type A character vector of dataset types to filter
#' @param verbose A logical value to print messages
#' @return A character vector of common dataset type
#' @keywords internal
get_common_dataset_type <- function(data_source, vec_dataset_type, verbose = TRUE) {
  .data <- rlang::.data

  assertthat_cli(
    inherits(data_source, "tbl"),
    msg = "{.arg data_source} must be a class of {.cls tbl}.",
    verbose = verbose
  )

  assertthat_cli(
    "dataset_type" %in% colnames(data_source),
    msg = "The dataset does not contain {.code dataset_type} column during the selection of common dataset type. Please check the column names of the dataset.",
    verbose = verbose
  )

  assertthat_cli(
    is.character(vec_dataset_type),
    msg = "{.arg vec_dataset_type} must be a character vector.",
    verbose = verbose
  )

  assertthat_cli(
    all(vec_dataset_type %in% c(
      "vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints"
    )),
    msg = "{.arg vec_dataset_type} must be one of {.code vegetation_plot}, {.code fossil_pollen_archive}, {.code traits}, or {.code gridpoints}.",
    verbose = verbose
  )

  assertthat_cli(
    inherits(verbose, "logical"),
    msg = "{.arg verbose} must be a logical value."
  )

  vec_present_dataset_type <-
    data_source %>%
    dplyr::distinct(.data$dataset_type) %>%
    dplyr::collect() %>%
    purrr::pluck(1)

  vec_common_dataset_type <-
    intersect(vec_present_dataset_type, vec_dataset_type)

  if (
    !all(vec_dataset_type %in% vec_present_dataset_type)
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_alert_warning(
        stringr::str_c(
          "The data does not contain all dataset types specified in {.arg vec_dataset_type}. ",
          "Changing {.arg vec_dataset_type} to the dataset types present in the data as: ",
          stringr::str_c(vec_common_dataset_type, collapse = ", ")
        )
      )
    }

    vec_dataset_type <- vec_common_dataset_type
  }

  if (
    !all(vec_present_dataset_type %in% vec_dataset_type)
  ) {
    dataset_type_missing <-
      setdiff(vec_present_dataset_type, vec_dataset_type)

    if (
      isTRUE(verbose)
    ) {
      cli::cli_alert_warning(
        stringr::str_glue(
          "The selected dataset type will not be filtered: {stringr::str_c(dataset_type_missing, collapse = ', ')}"
        )
      )
    }
  }

  return(vec_dataset_type)
}
