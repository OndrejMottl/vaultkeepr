#' @title Get readable column names
#' @description Get readable column names from the Vault database compilation.
#' Raw data in the vegVault contains many ID columns (e.g., `sample_id`,
#' `taxon_id`, `trait_id`, etc.). This function will automatically replace
#' these columns with more readable names (e.g., `sample_name`, `taxon_name`,
#' `trait_name`, etc.).
#' @param con A connection to the VegVault database.
#' @param data A tibble containing the raw data.
#' @return A tibble with readable column names.
#' @keywords internal
get_readable_column_names <- function(con, data) {
  assertthat::assert_that(
    inherits(data, "tbl"),
    msg = "data must be a class of `tbl`"
  )

  assertthat::assert_that(
    inherits(con, "SQLiteConnection"),
    msg = "path does not lead to valid SQLite database"
  )

  data_work <- data

  if (
    c(
      "dataset_id",
      "dataset_type_id",
      "dataset_name",
      "dataset_type"
    ) %in%
      colnames(data) %>%
      all() %>%
      isTRUE()
  ) {
    data_work <-
      data %>%
      dplyr::select(
        -c(
          "dataset_id",
          "dataset_type_id"
        )
      ) %>%
      dplyr::relocate(
        "dataset_name",
        "dataset_type"
      )
  }

  # Add column names to data_source -----
  add_column_from_db <- function(data_to_rename = NULL,
                                 sel_column_id = NA_character_,
                                 join_type = "left_join",
                                 sel_join_by = sel_column_id,
                                 sel_suffix = c("", ""),
                                 sel_column_name = NA_character_,
                                 table_name = NA_character_,
                                 sel_table = NULL,
                                 overwrite = FALSE) {
    if (
      isFALSE(sel_column_id %in% colnames(data_to_rename))
    ) {
      return(data_to_rename)
    }

    if (
      sel_column_name %in% colnames(data_to_rename) && isFALSE(overwrite)
    ) {
      res <-
        data_to_rename %>%
        dplyr::relocate(
          dplyr::all_of(sel_column_name),
          .after = dplyr::all_of(sel_column_id)
        ) %>%
        dplyr::select(-dplyr::all_of(sel_column_id))

      # pipe to return does not work, need to assign to res first
      return(res)
    }

    data_db <- sel_table
    if (
      isTRUE(is.null(sel_table))
    ) {
      data_db <-
        dplyr::tbl(con, table_name)
    }

    if (
      join_type == "left_join"
    ) {
      data_joined <-
        data_to_rename %>%
        dplyr::left_join(
          data_db,
          by = sel_join_by,
          suffix = sel_suffix
        )
    } else {
      data_joined <-
        data_to_rename %>%
        dplyr::inner_join(
          data_db,
          by = sel_join_by,
          suffix = sel_suffix
        )
    }

    res <-
      data_joined %>%
      dplyr::relocate(
        dplyr::all_of(sel_column_name),
        .after = dplyr::all_of(sel_column_id)
      ) %>%
      dplyr::select(-dplyr::all_of(sel_column_id))

    # pipe to return does not work, need to assign to res first
    return(res)
  }

  # create a reference table for all column names, which will be called to apply
  # the add_column_from_db function
  vec_column_id <-
    c(
      "data_source_id",
      "data_source_type_id",
      "sampling_method_id",
      "sample_id",
      "sample_size_id",
      "taxon_id",
      "trait_id",
      "trait_domain_id",
      "abiotic_variable_id"
    )

  data_table_ref <-
    tibble::tibble(
      sel_column_id = vec_column_id,
      sel_column_name = c(
        "data_source_desc",
        "dataset_source_type",
        "sampling_method_details",
        "sample_name",
        "sample_size",
        "taxon_name",
        "trait_name",
        "trait_domain_name",
        "abiotic_variable_name"
      ),
      table_name = c(
        "DatasetSourcesID",
        "DatasetSourceTypeID",
        "SamplingMethodID",
        "Samples",
        "SampleSizeID",
        "Taxa",
        "Traits",
        "TraitsDomain",
        "AbioticVariable"
      ),
      join_type = c(
        "left_join",
        "inner_join",
        "left_join",
        "inner_join",
        "left_join",
        "left_join",
        "left_join",
        "left_join",
        "left_join"
      )
    )

  # For loop is used as reduce function was too difficult to implement
  for (i in seq_along(vec_column_id)) {
    # print(i)

    data_ref_sub <-
      data_table_ref %>%
      dplyr::filter(.data$sel_column_id == vec_column_id[i])

    data_work <-
      add_column_from_db(
        data_to_rename = data_work,
        sel_column_id = vec_column_id[i],
        sel_column_name = data_ref_sub %>%
          purrr::chuck("sel_column_name", 1),
        table_name = data_ref_sub %>%
          purrr::chuck("table_name", 1),
        join_type = data_ref_sub %>%
          purrr::chuck("join_type", 1)
      )

    # data_work %>%
    #  dplyr::collect() %>%
    #  nrow() %>%
    #  print()
  }

  # add more complex columns manually -----

  data_work <-
    add_column_from_db(
      data_to_rename = data_work,
      sel_column_id = "taxon_id_trait",
      sel_column_name = "taxon_name",
      table_name = "Taxa",
      sel_join_by = c("taxon_id_trait" = "taxon_id"),
      sel_suffix = c("", "_trait"),
      overwrite = TRUE,
      join_type = "left_join"
    )

  data_work <-
    add_column_from_db(
      data_to_rename = data_work,
      sel_column_id = "sample_id_link",
      sel_column_name = "sample_name",
      sel_table = dplyr::tbl(con, "Samples") %>%
        dplyr::select(
          dplyr::all_of(c("sample_id", "sample_name"))
        ),
      sel_join_by = c("sample_id_link" = "sample_id"),
      sel_suffix = c("", "_gridpoint"),
      join_type = "left_join",
      overwrite = TRUE
    )

  return(data_work)
}
