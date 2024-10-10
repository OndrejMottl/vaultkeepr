pack_data <- function(
    sel_data = NULL,
    focus = c("vegetation", "traits"),
    verbose = TRUE) {
  assertthat::assert_that(
    inherits(sel_data, "tbl"),
    msg = "sel_data must be a class of `tbl`"
  )

  assertthat::assert_that(
    is.character(focus),
    msg = "The 'focus' must be a character"
  )

  focus <- match.arg(focus)


  assertthat::assert_that(
    focus %in% c("vegetation", "traits"),
    msg = "The 'focus' must be one of the following: 'vegetation', 'traits'"
  )

  assertthat::assert_that(
    is.logical(verbose),
    msg = "The 'verbose' must be a logical"
  )

  vec_dataset_cols <-
    intersect(
      colnames(sel_data),
      c(
        "dataset_name", "data_source_desc",
        "dataset_type", "dataset_source_type",
        "coord_long", "coord_lat",
        "sampling_method_details"
      )
    )

  if (
    focus == "vegetation"
  ) {
    data_packed <-
      sel_data %>%
      tidyr::nest(
        data_samples = c(
          "sample_name", "age",
          "sample_details", "sample_size", "description",
          "taxon_name", "value"
        )
      ) %>%
      dplyr::mutate(
        data_samples = purrr::map(
          .progress = ifelse(verbose, "processing taxa", FALSE),
          .x = .data$data_samples,
          .f = ~ .x %>%
            dplyr::relocate(
              dplyr::any_of(
                c(
                  "sample_name", "age",
                  "sample_details", "sample_size", "description"
                )
              )
            ) %>%
            dplyr::arrange(age, taxon_name) %>%
            tidyr::pivot_wider(
              names_from = "taxon_name",
              values_from = "value",
              values_fill = 0
            )
        )
      )
  } else {
    taxon_column_name <- "taxon_name"

    if (
      "taxon_name_trait" %in% colnames(sel_data)
    ) {
      taxon_column_name <- "taxon_name_trait"
    }

    data_packed <-
      sel_data %>%
      tidyr::nest(
        data_samples = c(
          "sample_name", "age",
          "sample_details", "sample_size", "description",
          "trait_domain_name", "trait_name",
          "taxon_name", "trait_value",
          "value"
        )
      ) %>%
      dplyr::mutate(
        data_samples = purrr::map(
          .progress = ifelse(verbose, "processing taxa", FALSE),
          .x = .data$data_samples,
          .f = ~ .x %>%
            dplyr::relocate(
              dplyr::any_of(
                c(
                  "sample_name", "age",
                  "sample_details", "sample_size", "description",
                  "trait_domain_name", "trait_name"
                )
              )
            ) %>%
            dplyr::arrange(.data$age, taxon_column_name) %>%
            tidyr::pivot_wider(
              names_from = taxon_column_name,
              values_from = "trait_value",
              values_fill = 0
            )
        )
      )
  }

  return(data_packed)
}
