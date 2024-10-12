#' @title Get references for selected VegVault Plan
#' @description This function extracts all references for selected
#' VegVault extraction plan (i,e., this function should be used before
#' extracting data from the plan).
#' @param con A VegVault connection (a plan).
#' @param type A character vector specifying the type of references to be
#' extracted. The following types are available: `Dataset`, `DatasetSource`,
#' `DatasetSourceType`, `SamplingMethod`, `Sample`, `Taxon`, `Trait`, and
#' `AbioticVariable`.
#' @return A tibble containing all references for the selected data compilation.
#' @export
get_references <- function(
    con = NULL,
    path = NULL,
    type = c(
      "Dataset",
      "DatasetSource",
      "DatasetSourceType",
      "SamplingMethod",
      "Sample",
      "Taxon",
      "Trait",
      "AbioticVariable"
    )) {
  .data <- rlang::.data
  `%>%` <- magrittr::`%>%`

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

  data_extracted_raw <-
    extract_data(
      con = con,
      return_raw_data = TRUE
    )

  assertthat::assert_that(
    inherits(type, "character"),
    msg = "data must be a class of `character`"
  )

  assertthat::assert_that(
    length(type) > 0,
    msg = "'type' must have at least one element"
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "path does not lead to valid SQLite database"
  )

  assertthat::assert_that(
    all(
      type %in% c(
        "Dataset",
        "DatasetSource",
        "DatasetSourceType",
        "SamplingMethod",
        "Sample",
        "Taxon",
        "Trait",
        "AbioticVariable"
      )
    ),
    msg = paste(
      "The 'type' must be one of the following:",
      "Dataset,
      DatasetSource,
      DatasetSourceType,
      SamplingMethod,
      Sample,
      Taxon,
      Trait,
      AbioticVariable"
    )
  )

  vec_refs <- NULL

  # helper function to get unique references for specific columns in a table
  get_uniq_refs <- function(data_source, sel_con, sel_column, sel_table) {
    vec_variable_id <-
      data_source %>%
      dplyr::distinct(!!dplyr::sym(sel_column)) %>%
      dplyr::collect() %>%
      purrr::chuck(sel_column)

    data_refs <-
      dplyr::tbl(sel_con, sel_table) %>%
      dplyr::filter(
        !!dplyr::sym(sel_column) %in% vec_variable_id
      ) %>%
      dplyr::distinct(.data$reference_id) %>%
      dplyr::collect() %>%
      purrr::chuck("reference_id")

    return(data_refs)
  }

  if (
    "Dataset" %in% type
  ) {
    assertthat::assert_that(
      "dataset_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `dataset_id` columns. Please make
        sure to to use `get_datasets()` to obtain the data"
      )
    )

    dataset_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "dataset_id",
        sel_table = "DatasetReferences"
      )

    vec_refs <-
      c(vec_refs, dataset_ref)
  }

  if (
    "DatasetSource" %in% type
  ) {
    assertthat::assert_that(
      "data_source_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `data_source_id` columns. Please make
        sure to to use `get_datasets()` to obtain the data"
      )
    )

    data_source_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "data_source_id",
        sel_table = "DatasetSourcesReference"
      )

    vec_refs <-
      c(vec_refs, data_source_ref)
  }

  if (
    "DatasetSourceType" %in% type
  ) {
    assertthat::assert_that(
      "data_source_type_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `data_source_type_id` columns. Please make
        sure to to use `get_datasets()` to obtain the data"
      )
    )

    data_source_type_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "data_source_type_id",
        sel_table = "DatasetSourceTypeReference"
      )

    vec_refs <-
      c(vec_refs, data_source_type_ref)
  }

  if (
    "SamplingMethod" %in% type
  ) {
    assertthat::assert_that(
      "sampling_method_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `sampling_method_id` columns. Please make
        sure to to use `get_datasets()` to obtain the data_extracted_raw"
      )
    )

    sampling_method_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "sampling_method_id",
        sel_table = "SamplingMethodReference"
      )

    vec_refs <-
      c(vec_refs, sampling_method_ref)
  }

  if (
    "Sample" %in% type
  ) {
    assertthat::assert_that(
      "sample_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `sample_id` columns. Please make
        sure to to use `get_samples()` to obtain the data"
      )
    )

    sample_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "sample_id",
        sel_table = "SampleReference"
      )

    vec_refs <-
      c(vec_refs, sample_ref)
  }

  if (
    "Taxon" %in% type
  ) {
    assertthat::assert_that(
      "taxon_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `taxon_id` columns. Please make
        sure to to use `get_taxa()` to obtain the data"
      )
    )

    taxon_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "taxon_id",
        sel_table = "TaxonReference"
      )

    vec_refs <-
      c(vec_refs, taxon_ref)
  }

  if (
    "Trait" %in% type
  ) {
    assertthat::assert_that(
      "trait_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `trait_id` columns. Please make
        sure to to use `get_traits()` to obtain the data"
      )
    )

    trait_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "trait_id",
        sel_table = "TraitsReference"
      )

    vec_refs <-
      c(vec_refs, trait_ref)
  }

  if (
    "AbioticVariable" %in% type
  ) {
    assertthat::assert_that(
      "abiotic_variable_id" %in% colnames(data_extracted_raw),
      msg = paste(
        "The dataset does not contain `abiotic_variable_id` columns. Please make
        sure to to use `get_abiotic_data()` to obtain the data"
      )
    )

    abiotic_variable_ref <-
      get_uniq_refs(
        data_source = data_extracted_raw,
        sel_con = sel_con,
        sel_column = "abiotic_variable_id",
        sel_table = "AbioticVariableReference"
      )

    vec_refs <-
      c(vec_refs, abiotic_variable_ref)
  }

  # extract all references
  res <-
    dplyr::tbl(sel_con, "References") %>%
    dplyr::filter(
      .data$reference_id %in% vec_refs
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  return(res)
}
