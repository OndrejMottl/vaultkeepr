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

  # Render the lazy selection once: every reference type below reads it as a
  #   subquery, and rendering the nested query takes about as long as running
  #   one reference query
  if (
    inherits(sel_data, "tbl_sql")
  ) {
    data_selection <-
      dplyr::tbl(
        src = sel_con,
        from = dplyr::sql(dbplyr::sql_render(sel_data)),
        vars = colnames(sel_data)
      )
  } else {
    data_selection <- sel_data
  }

  data_links_selected <-
    build_reference_link_table() |>
    dplyr::filter(
      .data[["reference_source"]] %in% type,
      .data[["sel_column"]] %in% colnames(sel_data)
    )

  # The ids of each type reach SQLite as a subquery, not as a literal list
  data_extracted_refs <-
    data_links_selected |>
    purrr::pmap(
      .f = ~ {
        sel_source <- ..1

        build_reference_query(
          data_source = data_selection,
          sel_con = sel_con,
          sel_column = ..2,
          sel_table = ..3
        ) |>
          dplyr::collect() |>
          dplyr::mutate(reference_source = sel_source)
      }
    ) |>
    purrr::list_rbind(
      ptype = tibble::tibble(
        reference_id = integer(),
        reference_detail = character(),
        mandatory = integer(),
        reference_source = character()
      )
    )

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

  res_full <-
    data_extracted_refs %>%
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

  res <-
    res_full |>
    dplyr::select("reference", "mandatory") |>
    dplyr::distinct()

  return(res)
}
