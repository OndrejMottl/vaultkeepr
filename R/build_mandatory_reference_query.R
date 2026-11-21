#' @title Build the query that finds mandatory references in a selection
#' @description
#' Build a lazy query over `data_source` that keeps the rows whose id
#' columns (those of [build_reference_link_table()] that `data_source`
#' holds) are linked to a reference with `mandatory = 1`, limited to one
#' row. The ids linked to mandatory references are given to SQLite as
#' subqueries, never as literal lists.
#' @param data_source
#' The `data` element of a `vault_pipe`: a lazy `tbl_sql`, or the empty
#' `tibble` that [open_vault()] creates.
#' @param sel_con
#' A `SQLiteConnection` to the Vault database.
#' @return
#' A lazy `tbl_sql` with at most one row, or `NULL` if `data_source` is not
#' a lazy table or holds none of the id columns.
#' @seealso [has_mandatory_references()], [build_reference_link_table()]
#' @keywords internal
build_mandatory_reference_query <- function(
  data_source,
  sel_con
) {
  data_links_present <-
    build_reference_link_table() |>
    dplyr::filter(
      .data[["sel_column"]] %in% colnames(data_source)
    )

  if (
    isFALSE(inherits(data_source, "tbl_sql")) ||
      nrow(data_links_present) == 0
  ) {
    return(NULL)
  }

  vec_column_present <-
    dplyr::pull(data_links_present, "sel_column")

  vec_table_present <-
    dplyr::pull(data_links_present, "sel_table")

  sql_mandatory_reference_id <-
    dplyr::tbl(
      src = sel_con,
      from = "References"
    ) |>
    dplyr::filter(
      .data[["mandatory"]] == 1L
    ) |>
    dplyr::select("reference_id") |>
    dbplyr::sql_render()

  # One condition per id column: the id is linked to a mandatory reference
  list_conditions <-
    purrr::map2(
      .x = vec_column_present,
      .y = vec_table_present,
      .f = ~ {
        sel_column <- .x
        sel_table <- .y

        sql_linked_id <-
          dplyr::tbl(
            src = sel_con,
            from = sel_table
          ) |>
          dplyr::filter(
            .data[["reference_id"]] %in%
              !!dplyr::sql(sql_mandatory_reference_id)
          ) |>
          dplyr::select(
            dplyr::all_of(sel_column)
          ) |>
          dbplyr::sql_render()

        rlang::expr(
          .data[[!!sel_column]] %in% !!dplyr::sql(sql_linked_id)
        )
      }
    )

  expr_any_mandatory <-
    list_conditions |>
    purrr::reduce(
      .f = ~ rlang::expr(!!.x | !!.y)
    )

  res <-
    data_source |>
    dplyr::filter(!!expr_any_mandatory) |>
    dplyr::select(
      dplyr::all_of(vec_column_present)
    ) |>
    utils::head(n = 1)

  return(res)
}
