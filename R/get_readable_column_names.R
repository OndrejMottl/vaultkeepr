# TODO add roxygen comments
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

  # Add column names to data_source -----
  add_column_from_db <- function(data = NULL, sel_column_id = NA_character_,
                                 sel_join_by = sel_column_id,
                                 sel_suffix = c("", ""),
                                 sel_column_name = NA_character_,
                                 table_name = NA_character_,
                                 sel_table = NULL,
                                 overwrite = FALSE) {
    if (
      sel_column_id %in% colnames(data)
    ) {
      if (
        sel_column_name %in% colnames(data) && overwrite == FALSE
      ) {
        res <-
          data %>%
          dplyr::relocate(
            dplyr::all_of(sel_column_name),
            .after = dplyr::all_of(sel_column_id)
          ) %>%
          dplyr::select(-dplyr::all_of(sel_column_id))
      } else {
        if (
          isFALSE(is.null(sel_table))
        ) {
          data_db <- sel_table
        } else {
          data_db <-
            dplyr::tbl(con, table_name)
        }

        res <-
          data %>%
          dplyr::left_join(
            data_db,
            by = sel_join_by,
            suffix = sel_suffix
          ) %>%
          dplyr::relocate(
            dplyr::all_of(sel_column_name),
            .after = dplyr::all_of(sel_column_id)
          ) %>%
          dplyr::select(-dplyr::all_of(sel_column_id))
      }
      return(res)
    }
    return(data)
  }

  # create a reference table for all column names, which will be called to apply
  # the add_column_from_db function
  vec_column_id <-
    c(
      "dataset_id",
      "data_source_id",
      "dataset_type_id",
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
        "dataset_name",
        "data_source_desc",
        "dataset_type",
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
        "Datasets",
        "DatasetSourcesID",
        "DatasetTypeID",
        "DatasetSourceTypeID",
        "SamplingMethodID",
        "Samples",
        "SampleSizeID",
        "Taxa",
        "Traits",
        "TraitsDomain",
        "AbioticVariable"
      )
    )

  # For loop is used as reduce function was too difficult to implement
  for (i in seq_along(vec_column_id)) {
    data_ref_sub <-
      data_table_ref %>%
      dplyr::filter(.data$sel_column_id == vec_column_id[i])

    data_work <-
      add_column_from_db(
        data = data_work,
        sel_column_id = vec_column_id[i],
        sel_column_name = data_ref_sub %>%
          purrr::chuck("sel_column_name", 1),
        table_name = data_ref_sub %>%
          purrr::chuck("table_name", 1)
      )
  }

  # add more complex columns manually -----

  data_work <-
    add_column_from_db(
      data = data_work,
      sel_column_id = "taxon_id_trait",
      sel_column_name = "taxon_name",
      table_name = "Taxa",
      sel_join_by = c("taxon_id_trait" = "taxon_id"),
      sel_suffix = c("", "_trait"),
      overwrite = TRUE
    )

  data_work <-
    add_column_from_db(
      data = data_work,
      sel_column_id = "sample_id_link",
      sel_column_name = "sample_name",
      sel_table = dplyr::tbl(con, "Samples") %>%
        dplyr::select(
          dplyr::all_of(c("sample_id", "sample_name"))
        ),
      sel_join_by = c("sample_id_link" = "sample_id"),
      sel_suffix = c("", "_gridpoint"),
      overwrite = TRUE
    )

  return(data_work)
}
