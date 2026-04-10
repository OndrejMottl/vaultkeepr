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
#' @param get_source A logical indicating whether to extract the source of each
#' references
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
    get_source = TRUE,
    verbose = TRUE) {
  .data <- rlang::.data
  `%>%` <- magrittr::`%>%`

  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a class of {.cls vault_pipe}. Use {.fn open_vault} to create a connection.",
    verbose = verbose
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con}. Use {.fn open_vault} to create a connection.",
    verbose = verbose
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code data} must be a class of {.cls tbl}.",
    verbose = verbose
  )

  assertthat_cli(
    inherits(type, "character"),
    msg = "{.arg type} must be a character vector.",
    verbose = verbose
  )

  assertthat_cli(
    length(type) > 0,
    msg = "{.arg type} must have at least one element.",
    verbose = verbose
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.arg con} does not contain a valid SQLite database connection.",
    verbose = verbose
  )

  assertthat_cli(
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
    msg = "{.arg type} must be one of {.code Dataset}, {.code DatasetSource}, {.code DatasetSourceType}, {.code SamplingMethod}, {.code Sample}, {.code Taxon}, {.code Trait}, {.code AbioticVariable}.",
    verbose = verbose
  )

  assertthat_cli(
    is.logical(get_source),
    msg = "{.arg get_source} must be a logical value.",
    verbose = verbose
  )

  assertthat_cli(
    is.logical(verbose),
    msg = "{.arg verbose} must be a logical value."
  )

  data_extracted_refs <-
    tibble::tibble(
      reference_id = integer(),
      reference_source = character()
    )

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
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "dataset_id",
            sel_table = "DatasetReferences"
          ),
          reference_source = "Dataset"
        )
      )
  }

  if (
    "DatasetSource" %in% type &&
      "data_source_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "data_source_id",
            sel_table = "DatasetSourcesReference"
          ),
          reference_source = "DatasetSource"
        )
      )
  }

  if (
    "DatasetSourceType" %in% type &&
      "data_source_type_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "data_source_type_id",
            sel_table = "DatasetSourceTypeReference"
          ),
          reference_source = "DatasetSourceType"
        )
      )
  }

  if (
    "SamplingMethod" %in% type &&
      "sampling_method_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "sampling_method_id",
            sel_table = "SamplingMethodReference"
          ),
          reference_source = "SamplingMethod"
        )
      )
  }

  if (
    "Sample" %in% type &&
      "sample_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "sample_id",
            sel_table = "SampleReference"
          ),
          reference_source = "Sample"
        )
      )
  }

  if (
    "Taxon" %in% type &&
      "taxon_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "taxon_id",
            sel_table = "TaxonReference"
          ),
          reference_source = "Taxon"
        )
      )
  }

  if (
    "Trait" %in% type &&
      "trait_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "trait_id",
            sel_table = "TraitsReference"
          ),
          reference_source = "Trait"
        )
      )
  }

  if (
    "AbioticVariable" %in% type &&
      "abiotic_variable_id" %in% colnames(sel_data)
  ) {
    data_extracted_refs <-
      data_extracted_refs %>%
      dplyr::bind_rows(
        dplyr::tibble(
          reference_id = get_uniq_refs(
            data_source = sel_data,
            sel_con = sel_con,
            sel_column = "abiotic_variable_id",
            sel_table = "AbioticVariableReference"
          ),
          reference_source = "AbioticVariable"
        )
      )
  }

  if (
    nrow(data_extracted_refs) == 0
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_alert_warning("No references found for the selected data compilation.")
    }

    return(NULL)
  }

  # get unique references
  vec_refs <-
    data_extracted_refs %>%
    dplyr::distinct(.data$reference_id) %>%
    purrr::chuck("reference_id")

  # extract selected references
  data_ref_db <-
    dplyr::tbl(sel_con, "References") %>%
    dplyr::filter(
      .data$reference_id %in% vec_refs
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect()


  res_full <-
    data_extracted_refs %>%
    dplyr::left_join(
      data_ref_db,
      by = c("reference_id" = "reference_id")
    ) %>%
    dplyr::mutate(
      mandatory = as.logical(.data$mandatory)
    ) %>%
    dplyr::select("reference_detail", "reference_source", "mandatory") %>%
    rlang::set_names(
      nm = c(
        "reference",
        "reference_source",
        "mandatory"
      )
    ) %>%
    dplyr::distinct()

  if (
    isTRUE(verbose)
  ) {
    vec_refs_collapse <-
      res_full %>%
      dplyr::distinct(.data$reference_source) %>%
      purrr::chuck("reference_source") %>%
      stringr::str_c(collapse = ", ")

    cli::cli_alert_info(
      stringr::str_glue(
        "References for the following types have been extracted: {vec_refs_collapse}"
      )
    )
  }

  if (
    isTRUE(get_source)
  ) {
    return(res_full)
  }

  res_full %>%
    dplyr::select("reference", "mandatory") %>%
    dplyr::distinct() %>%
    return()
}
