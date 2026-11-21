#' @title Build the SQL of the distinct ids of a selection
#' @description
#' Render the distinct values of one id column of a lazy selection as SQL,
#' for use as a subquery in `%in%`. SQLite then reads the ids of the
#' selection itself: they are never collected into R or sent back as a
#' literal list.
#' @param data_source
#' A lazy `tbl_sql` that holds the column `sel_column`.
#' @param sel_column
#' Name of the id column, for example `"sample_id"`.
#' @return
#' A `sql` object holding `SELECT DISTINCT <sel_column> FROM ...`, to be
#' used as `dplyr::filter(.data[[sel_column]] %in% !!res)`.
#' @details
#' `DISTINCT` lets SQLite skip the joins of `data_source` that do not change
#' the ids. SQLite evaluates such an `IN` subquery once, not once per row.
#' @seealso [build_age_uncertainty_query()]
#' @keywords internal
build_selected_id_sql <- function(
  data_source,
  sel_column
) {
  assertthat_cli(
    exp = inherits(data_source, "tbl_sql"),
    msg = "{.arg data_source} must be a lazy {.cls tbl_sql}"
  )

  assertthat_cli(
    exp = is.character(sel_column) && length(sel_column) == 1,
    msg = "{.arg sel_column} must be a single column name"
  )

  assertthat_cli(
    exp = sel_column %in% colnames(data_source),
    msg = stringr::str_c(
      "{.arg data_source} does not contain the column ",
      "{.code ", sel_column, "}"
    )
  )

  res <-
    data_source |>
    dplyr::distinct(
      dplyr::across(
        dplyr::all_of(sel_column)
      )
    ) |>
    dbplyr::sql_render()

  return(res)
}
