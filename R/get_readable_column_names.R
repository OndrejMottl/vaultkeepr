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
  assertthat_cli(
    inherits(data, "tbl"),
    msg = "{.arg data} must be a {.cls tbl}"
  )

  assertthat_cli(
    inherits(con, "SQLiteConnection"),
    msg = "{.arg con} does not lead to a valid {.cls SQLiteConnection}"
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
  # add_column_from_db() replaces each id column with its readable column

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
        con = con,
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
      con = con,
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
      con = con,
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
