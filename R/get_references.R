#' @title Get references for selected VegVault Plan
#' @description This function extracts all references for selected
#' VegVault extraction plan (i,e., this function should be used before
#' extracting data from the plan).
#' @param con A VegVault connection (a plan).
#' @param type A character vector specifying the type of references to be
#' extracted. The function will try to extract all possible reference types.
#' The following types are available: `Dataset`, `DatasetSource`,
#' `DatasetSourceType`, `SamplingMethod`, `Sample`, `Taxon`, `Trait`, and
#' `AbioticVariable`.
#' @param verbose A logical indicating whether to print messages about the
#' progress of the function. Default is `TRUE`.
#' @return A tibble containing all references for the selected data compilation.
#' @export
get_references <- function(
    con = NULL,
    type = c(
      "Dataset",
      "DatasetSource",
      "DatasetSourceType",
      "SamplingMethod",
      "Sample",
      "Taxon",
      "Trait",
      "AbioticVariable"
    ),
    verbose = TRUE) {
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

  sel_data <- con$data

  assertthat::assert_that(
    inherits(sel_data, "tbl"),
    msg = "data must be a class of `tbl`"
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

  assertthat::assert_that(
    is.logical(verbose),
    msg = "'verbose' must be a class of `logical`"
  )

  vec_refs <- NULL
  vec_types_present <- NULL

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
    "Dataset" %in% type &&
      "dataset_id" %in% colnames(sel_data)
  ) {
    dataset_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "dataset_id",
        sel_table = "DatasetReferences"
      )

    vec_refs <-
      c(vec_refs, dataset_ref)

    vec_types_present <-
      c(vec_types_present, "Dataset")
  }

  if (
    "DatasetSource" %in% type &&
      "data_source_id" %in% colnames(sel_data)
  ) {
    data_source_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "data_source_id",
        sel_table = "DatasetSourcesReference"
      )

    vec_refs <-
      c(vec_refs, data_source_ref)

    vec_types_present <-
      c(vec_types_present, "DatasetSource")
  }

  if (
    "DatasetSourceType" %in% type &&
      "data_source_type_id" %in% colnames(sel_data)
  ) {
    data_source_type_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "data_source_type_id",
        sel_table = "DatasetSourceTypeReference"
      )

    vec_refs <-
      c(vec_refs, data_source_type_ref)

    vec_types_present <-
      c(vec_types_present, "DatasetSourceType")
  }

  if (
    "SamplingMethod" %in% type &&
      "sampling_method_id" %in% colnames(sel_data)
  ) {
    sampling_method_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "sampling_method_id",
        sel_table = "SamplingMethodReference"
      )

    vec_refs <-
      c(vec_refs, sampling_method_ref)

    vec_types_present <-
      c(vec_types_present, "SamplingMethod")
  }

  if (
    "Sample" %in% type &&
      "sample_id" %in% colnames(sel_data)
  ) {
    sample_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "sample_id",
        sel_table = "SampleReference"
      )

    vec_refs <-
      c(vec_refs, sample_ref)

    vec_types_present <-
      c(vec_types_present, "Sample")
  }

  if (
    "Taxon" %in% type &&
      "taxon_id" %in% colnames(sel_data)
  ) {
    taxon_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "taxon_id",
        sel_table = "TaxonReference"
      )

    vec_refs <-
      c(vec_refs, taxon_ref)

    vec_types_present <-
      c(vec_types_present, "Taxon")
  }

  if (
    "Trait" %in% type &&
      "trait_id" %in% colnames(sel_data)
  ) {
    trait_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "trait_id",
        sel_table = "TraitsReference"
      )

    vec_refs <-
      c(vec_refs, trait_ref)

    vec_types_present <-
      c(vec_types_present, "Trait")
  }

  if (
    "AbioticVariable" %in% type &&
      "abiotic_variable_id" %in% colnames(sel_data)
  ) {
    abiotic_variable_ref <-
      get_uniq_refs(
        data_source = sel_data,
        sel_con = sel_con,
        sel_column = "abiotic_variable_id",
        sel_table = "AbioticVariableReference"
      )

    vec_refs <-
      c(vec_refs, abiotic_variable_ref)

    vec_types_present <-
      c(vec_types_present, "AbioticVariable")
  }

  if (
    length(vec_refs) == 0
  ) {
    if (
      isTRUE(verbose)
    ) {
      message("No references found for the selected data compilation.")
    }

    return(NULL)
  }

  # extract all references
  res <-
    dplyr::tbl(sel_con, "References") %>%
    dplyr::filter(
      .data$reference_id %in% vec_refs
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  if (
    isTRUE(verbose)
  ) {
    message(
      paste(
        "References for the following types have been extracted:",
        paste(vec_types_present, collapse = ", ")
      )
    )
  }


  return(res)
}
