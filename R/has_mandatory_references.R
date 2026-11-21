#' @title Check whether a selection holds a mandatory reference
#' @description
#' Decide whether the selection of a `vault_pipe` holds any record that is
#' linked to a reference marked as mandatory. The check runs in SQLite as
#' one query that stops at the first match: the selection is never collected
#' into R, and no literal list of ids is sent back to SQLite.
#' @param data_source
#' The `data` element of a `vault_pipe`: a lazy `tbl_sql`, or the empty
#' `tibble` that [open_vault()] creates.
#' @param sel_con
#' A `SQLiteConnection` to the Vault database.
#' @return
#' `TRUE` if at least one row of `data_source` has an id linked to a
#' mandatory reference, otherwise `FALSE`.
#' @seealso [build_mandatory_reference_query()], [extract_data()]
#' @keywords internal
has_mandatory_references <- function(
  data_source,
  sel_con
) {
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
    data_query |>
    dplyr::collect() |>
    nrow()

  res <-
    n_rows_mandatory > 0

  return(res)
}
