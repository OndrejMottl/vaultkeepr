#' @title Build the query for the age uncertainty of a selection
#' @description
#' Build a lazy query of the rows of `SampleUncertainty` whose `sample_id` is
#' in the selection `data_source`. The sample ids reach SQLite as the
#' subquery `sample_id IN (SELECT DISTINCT sample_id FROM ...)`, which SQLite
#' evaluates once and serves through the index or the primary key on
#' `SampleUncertainty.sample_id`.
#' @param data_source
#' A lazy `tbl_sql` that holds the column `sample_id`.
#' @param sel_con
#' A `SQLiteConnection` to the Vault database.
#' @return
#' A lazy `tbl_sql` with the columns of `SampleUncertainty`, its `age`
#' column renamed to `age_uncertainty`: `sample_id`, `iteration` and
#' `age_uncertainty`. The rows are in no guaranteed order.
#' @seealso [get_age_uncertainty()], [build_selected_id_sql()]
#' @keywords internal
build_age_uncertainty_query <- function(
  data_source,
  sel_con
) {
  sql_selected_sample_id <-
    build_selected_id_sql(
      data_source = data_source,
      sel_column = "sample_id"
    )

  res <-
    dplyr::tbl(
      src = sel_con,
      from = "SampleUncertainty"
    ) |>
    dplyr::filter(
      .data[["sample_id"]] %in% !!sql_selected_sample_id
    ) |>
    dplyr::rename(age_uncertainty = "age")

  return(res)
}
