#' @title Pack the data into nested tibbles
#' @description Pack the data into nested tibbles for easier access and
#' manipulation. Only specific columns are selected and packed into the
#' following nested tibbles:
#' - `data_samples` for sample metadata
#' - `data_community` for community (vegetation) data
#' - `data_traits` for trait data
#' - `data_abiotic` for abiotic data
#' When the `sel_data` does not contain the required columns, the function
#' will skip the respective data type.
#' @param sel_data A tibble with data
#' @param verbose A logical indicating whether to output additional information.
#' Default is `TRUE`.
#' @return A tibble with nested data.frames
#' @keywords internal
pack_data <- function(
    sel_data = NULL,
    verbose = TRUE) {
  assertthat::assert_that(
    inherits(sel_data, "tbl"),
    msg = "sel_data must be a class of `tbl`"
  )

  assertthat::assert_that(
    is.logical(verbose),
    msg = "The 'verbose' must be a logical"
  )

  # dataset data -----
  vec_dataset_cols <-
    c(
      "dataset_name", "data_source_desc",
      "dataset_type", "dataset_source_type",
      "coord_long", "coord_lat",
      "sampling_method_details"
    )

  if (
    !all(vec_dataset_cols %in% colnames(sel_data))
  ) {
    return(
      tibble::tibble()
    )
  }

  data_packed <-
    sel_data %>%
    dplyr::select(
      dplyr::all_of(vec_dataset_cols)
    ) %>%
    dplyr::distinct()

  # sample metadata -----
  vec_sample_meta_cols <-
    c(
      "sample_name", "age",
      "sample_details", "sample_size", "description"
    )

  if (
    all(vec_sample_meta_cols %in% colnames(sel_data))
  ) {
    data_samples_meta <-
      sel_data %>%
      dplyr::select(
        "dataset_name",
        dplyr::any_of(vec_sample_meta_cols)
      ) %>%
      dplyr::arrange(.data$age) %>%
      dplyr::distinct() %>%
      tidyr::nest(
        data_samples = dplyr::any_of(vec_sample_meta_cols)
      )

    data_packed <-
      dplyr::left_join(
        data_packed,
        data_samples_meta,
        by = "dataset_name"
      )
  }

  # community data -----
  vec_sample_community_cols <-
    c(
      "sample_name",
      "taxon_name", "value"
    )

  if (
    all(vec_sample_community_cols %in% colnames(sel_data))
  ) {
    data_samples_community <-
      sel_data %>%
      dplyr::select(
        "dataset_name",
        dplyr::any_of(vec_sample_community_cols)
      ) %>%
      tidyr::drop_na("value") %>%
      tidyr::nest(
        data_community = dplyr::all_of(vec_sample_community_cols)
      ) %>%
      dplyr::mutate(
        data_community = purrr::map(
          .progress = ifelse(verbose, "Packing community data", FALSE),
          .x = .data$data_community,
          .f = ~ .x %>%
            dplyr::group_by(.data$sample_name, .data$taxon_name) %>%
            dplyr::summarise(
              .groups = "drop",
              value = sum(.data$value)
            ) %>%
            tidyr::pivot_wider(
              names_from = "taxon_name",
              values_from = "value",
              values_fill = 0
            )
        )
      )

    data_packed <-
      dplyr::left_join(
        data_packed,
        data_samples_community,
        by = "dataset_name"
      )
  }

  # trait data -----
  vec_samples_trait_cols <-
    c(
      "sample_name",
      "trait_domain_name", "trait_name",
      "trait_value",
      ifelse(
        "taxon_name_trait" %in% colnames(sel_data),
        "taxon_name_trait",
        "taxon_name"
      )
    )

  if (
    all(vec_samples_trait_cols %in% colnames(sel_data))
  ) {
    data_samples_traits <-
      sel_data %>%
      dplyr::select(
        "dataset_name",
        dplyr::any_of(vec_samples_trait_cols)
      ) %>%
      tidyr::drop_na("trait_value") %>%
      tidyr::nest(
        data_traits = dplyr::any_of(vec_samples_trait_cols)
      ) %>%
      dplyr::mutate(
        data_traits = purrr::map(
          .progress = ifelse(verbose, "Packing trait data", FALSE),
          .x = .data$data_traits,
          .f = ~ .x %>%
            tidyr::pivot_wider(
              names_from = ifelse(
                "taxon_name_trait" %in% colnames(sel_data),
                "taxon_name_trait",
                "taxon_name"
              ),
              values_from = "trait_value",
              values_fill = NA_real_
            )
        )
      )

    data_packed <-
      dplyr::left_join(
        data_packed,
        data_samples_traits,
        by = "dataset_name"
      )
  }

  # abiotic data -----
  vec_sample_abiotic_cols <-
    c(
      "abiotic_variable_name", "abiotic_value",
      "abiotic_variable_unit", "measure_details",
      "sample_name_gridpoint"
    )

  if (
    all(vec_sample_abiotic_cols %in% colnames(sel_data))
  ) {
    data_abiotic_raw <-
      sel_data %>%
      dplyr::select(
        "dataset_name",
        "sample_name",
        dplyr::any_of(vec_sample_abiotic_cols)
      ) %>%
      tidyr::drop_na("abiotic_value") %>%
      dplyr::relocate("sample_name_gridpoint")

    data_abiotic <-
      sel_data %>%
      dplyr::distinct(.data$dataset_name, .data$sample_name) %>%
      dplyr::inner_join(
        data_abiotic_raw,
        by = dplyr::join_by("sample_name" == "sample_name_gridpoint"),
        suffix = c("", "_abiotic")
      ) %>%
      tidyr::nest(
        data_abiotic = dplyr::any_of(
          c(
            "sample_name",
            "dataset_name_abiotic", "sample_name_abiotic",
            vec_sample_abiotic_cols
          )
        )
      )

    data_packed <-
      dplyr::left_join(
        data_packed,
        data_abiotic,
        by = "dataset_name"
      )
  }

  return(data_packed)
}
