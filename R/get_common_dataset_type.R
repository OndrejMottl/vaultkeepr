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

  assertthat::assert_that(
    inherits(data_source, "tbl"),
    msg = "data_source must be a class of `tbl`"
  )

  assertthat::assert_that(
    "dataset_type" %in% colnames(data_source),
    msg = paste(
      "The dataset does not contain `dataset_type` columns",
      "during the selection of common dataset type.",
      "Please check the column names of the dataset."
    )
  )

  assertthat::assert_that(
    is.character(vec_dataset_type),
    msg = "`vec_dataset_type` must be a character vector"
  )

  assertthat::assert_that(
    all(vec_dataset_type %in% c(
      "vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints"
    )),
    msg = paste(
      "The `vec_dataset_type` must be one of the following:",
      "`vegetation_plot`, `fossil_pollen_archive`, `traits`, `gridpoints`"
    )
  )

  assertthat::assert_that(
    inherits(verbose, "logical"),
    msg = "`verbose` must be a logical value"
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
      message(
        paste(
          "The data does not contain the all dataset types specified in",
          "`vec_dataset_type`.", "\n",
          "Changing `vec_dataset_type` to the dataset types",
          "present in the data as:",
          paste(vec_common_dataset_type, collapse = ", ")
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
      message(
        paste(
          "The selected dataset type will not be filtered:",
          paste(dataset_type_missing, collapse = ", ")
        )
      )
    }
  }

  return(vec_dataset_type)
}
