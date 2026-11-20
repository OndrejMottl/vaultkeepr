#' @title Build the query for the references linked to a selection
#' @description
#' Build a lazy query that returns the references linked, through the link
#' table `sel_table`, to the ids in the column `sel_column` of
#' `data_source`. The ids are given to SQLite as a subquery of the distinct
#' ids of `data_source`, never as a literal list. `DISTINCT` lets SQLite skip
#' the joins of `data_source` that do not change the ids.
#' @param data_source A lazy `tbl_sql` that holds the column `sel_column`.
#' @param sel_con A `SQLiteConnection` to the Vault database.
#' @param sel_column Name of the id column, for example `"dataset_id"`.
#' @param sel_table
#' Name of the link table between `sel_column` and `References`, for
#' example `"DatasetReferences"`.
#' @return
#' A lazy `tbl_sql` with one row per distinct linked reference and the
#' columns `reference_id`, `reference_detail` and `mandatory`. A reference
#' id of the link table that is missing from `References` keeps `NA` in
#' `reference_detail` and `mandatory`.
#' @seealso [get_references()]
#' @keywords internal
build_reference_query <- function(
    data_source,
    sel_con,
    sel_column,
    sel_table) {
  sql_selected_id <-
    data_source %>%
    dplyr::distinct(
      dplyr::across(
        dplyr::all_of(sel_column)
      )
    ) %>%
    dbplyr::sql_render()

  data_references <-
    dplyr::tbl(sel_con, "References") %>%
    dplyr::select("reference_id", "reference_detail", "mandatory")

  res <-
    dplyr::tbl(sel_con, sel_table) %>%
    dplyr::filter(
      !!rlang::sym(sel_column) %in% !!dplyr::sql(sql_selected_id)
    ) %>%
    dplyr::distinct(.data$reference_id) %>%
    dplyr::left_join(
      data_references,
      by = "reference_id"
    )

  return(res)
}
