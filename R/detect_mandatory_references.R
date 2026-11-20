#' @title Detect mandatory references in a selection
#' @description
#' Decide whether the selection of a `vault_pipe` holds any record that is
#' linked to a reference marked as mandatory. The check runs in SQLite as
#' one query that stops at the first match: the selection is never collected
#' into R, and no literal list of ids is sent back to SQLite.
#' @param data_source
#' The `data` element of a `vault_pipe`: a lazy `tbl_sql`, or the empty
#' `tibble` that [open_vault()] creates.
#' @param sel_con A `SQLiteConnection` to the Vault database.
#' @return
#' `TRUE` if at least one row of `data_source` has an id linked to a
#' mandatory reference, otherwise `FALSE`.
#' @seealso [build_mandatory_reference_query()], [extract_data()]
#' @keywords internal
detect_mandatory_references <- function(data_source, sel_con) {
  data_query <-
    build_mandatory_reference_query(
      data_source = data_source,
      sel_con = sel_con
    )

  if (
    is.null(data_query)
  ) {
    return(FALSE)
  }

  n_rows_mandatory <-
    data_query %>%
    dplyr::collect() %>%
    nrow()

  res <- n_rows_mandatory > 0

  return(res)
}

#' @title Build the query that finds mandatory references in a selection
#' @description
#' Build a lazy query over `data_source` that keeps the rows whose id
#' columns (`dataset_id`, `data_source_id`, `data_source_type_id`,
#' `sampling_method_id`, `sample_id`, `taxon_id`, `trait_id` and
#' `abiotic_variable_id`, where present) are linked to a reference with
#' `mandatory = 1`, limited to one row. The ids linked to mandatory
#' references are given to SQLite as subqueries, never as literal lists.
#' @inheritParams detect_mandatory_references
#' @return
#' A lazy `tbl_sql` with at most one row, or `NULL` if `data_source` holds
#' none of the id columns.
#' @seealso [detect_mandatory_references()]
#' @keywords internal
build_mandatory_reference_query <- function(data_source, sel_con) {
  data_links <-
    tibble::tribble(
      ~sel_column, ~sel_table,
      "dataset_id", "DatasetReferences",
      "data_source_id", "DatasetSourcesReference",
      "data_source_type_id", "DatasetSourceTypeReference",
      "sampling_method_id", "SamplingMethodReference",
      "sample_id", "SampleReference",
      "taxon_id", "TaxonReference",
      "trait_id", "TraitsReference",
      "abiotic_variable_id", "AbioticVariableReference"
    )

  data_links_present <-
    data_links %>%
    dplyr::filter(
      .data$sel_column %in% colnames(data_source)
    )

  if (
    isFALSE(inherits(data_source, "tbl_sql")) ||
      nrow(data_links_present) == 0
  ) {
    return(NULL)
  }

  sql_mandatory_reference_id <-
    dplyr::tbl(sel_con, "References") %>%
    dplyr::filter(.data$mandatory == 1L) %>%
    dplyr::select("reference_id") %>%
    dbplyr::sql_render()

  # one condition per id column: the id is linked to a mandatory reference
  list_conditions <-
    purrr::map2(
      .x = dplyr::pull(data_links_present, "sel_column"),
      .y = dplyr::pull(data_links_present, "sel_table"),
      .f = function(sel_column, sel_table) {
        sql_linked_id <-
          dplyr::tbl(sel_con, sel_table) %>%
          dplyr::filter(
            .data$reference_id %in% !!dplyr::sql(sql_mandatory_reference_id)
          ) %>%
          dplyr::select(
            dplyr::all_of(sel_column)
          ) %>%
          dbplyr::sql_render()

        rlang::expr(
          !!rlang::sym(sel_column) %in% !!dplyr::sql(sql_linked_id)
        )
      }
    )

  expr_any_mandatory <-
    purrr::reduce(
      .x = list_conditions,
      .f = function(expr_left, expr_right) {
        rlang::expr(!!expr_left | !!expr_right)
      }
    )

  res <-
    data_source %>%
    dplyr::filter(!!expr_any_mandatory) %>%
    dplyr::select(
      dplyr::all_of(
        dplyr::pull(data_links_present, "sel_column")
      )
    ) %>%
    utils::head(1)

  return(res)
}
