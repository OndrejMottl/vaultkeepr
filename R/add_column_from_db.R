#' @title Replace an id column with a readable column from the database
#' @description
#' Join a lookup table of the database to the data and replace an id column
#' with the readable column of that table, for example `trait_domain_id`
#' with `trait_domain_name`. Used by [get_readable_column_names()].
#' @param con
#' A valid `DBI` connection to the VegVault database, used to read
#' `table_name` when `sel_table` is `NULL`.
#' @param data_to_rename
#' The data, a lazy table or a data frame.
#' @param sel_column_id
#' Character. The id column to replace.
#' @param join_type
#' Character. `"left_join"` keeps every row of the data; `"inner_join"`
#' keeps only the rows with a match in the lookup table.
#' @param sel_join_by
#' The join columns, as for `dplyr::left_join()`. Defaults to
#' `sel_column_id`.
#' @param sel_suffix
#' Character of length 2. The suffixes for columns that are in both the data
#' and the lookup table, as for `dplyr::left_join()`.
#' @param sel_column_name
#' Character. The readable column of the lookup table.
#' @param table_name
#' Character. The table of the database to join, when `sel_table` is `NULL`.
#' @param sel_table
#' A lazy table to join instead of `table_name`, or `NULL`.
#' @param overwrite
#' Logical. If `FALSE` (default) and the data already have
#' `sel_column_name`, no table is joined: the existing column is kept and
#' only the id column is removed.
#' @return
#' The data without `sel_column_id`, with `sel_column_name` in its place and
#' the other columns of the lookup table added at the end.
#' @details
#' The data are returned unchanged when they do not have `sel_column_id`.
#'
#' When the two suffixes are the same, a column of the lookup table that the
#' data already have, other than the join columns, is not joined: the data
#' keep their own. Without this, the join would create two columns of the
#' same name. In VegVault 2.0.0, for example, the trait values carry
#' `trait_unit`, and so does `TraitsDomain`.
#' @seealso [get_readable_column_names()]
#' @keywords internal
add_column_from_db <- function(
  con = NULL,
  data_to_rename = NULL,
  sel_column_id = NA_character_,
  join_type = "left_join",
  sel_join_by = sel_column_id,
  sel_suffix = c("", ""),
  sel_column_name = NA_character_,
  table_name = NA_character_,
  sel_table = NULL,
  overwrite = FALSE
) {
  if (
    isFALSE(sel_column_id %in% colnames(data_to_rename))
  ) {
    return(data_to_rename)
  }

  if (
    sel_column_name %in% colnames(data_to_rename) && isFALSE(overwrite)
  ) {
    res <-
      data_to_rename %>%
      dplyr::relocate(
        dplyr::all_of(sel_column_name),
        .after = dplyr::all_of(sel_column_id)
      ) %>%
      dplyr::select(-dplyr::all_of(sel_column_id))

    return(res)
  }

  data_db <-
    if (
      isTRUE(is.null(sel_table))
    ) {
      dplyr::tbl(
        src = con,
        from = table_name
      )
    } else {
      sel_table
    }

  # With equal suffixes, a column in both the data and the lookup table
  #   would be duplicated; the data keep their own
  vec_join_columns <-
    c(names(sel_join_by), unname(sel_join_by))

  vec_columns_present <-
    base::setdiff(
      base::intersect(colnames(data_db), colnames(data_to_rename)),
      vec_join_columns
    )

  data_db_selected <-
    if (
      sel_suffix[1] == sel_suffix[2] && length(vec_columns_present) > 0
    ) {
      data_db %>%
        dplyr::select(-dplyr::all_of(vec_columns_present))
    } else {
      data_db
    }

  if (
    join_type == "left_join"
  ) {
    data_joined <-
      data_to_rename %>%
      dplyr::left_join(
        data_db_selected,
        by = sel_join_by,
        suffix = sel_suffix
      )
  } else {
    data_joined <-
      data_to_rename %>%
      dplyr::inner_join(
        data_db_selected,
        by = sel_join_by,
        suffix = sel_suffix
      )
  }

  res <-
    data_joined %>%
    dplyr::relocate(
      dplyr::all_of(sel_column_name),
      .after = dplyr::all_of(sel_column_id)
    ) %>%
    dplyr::select(-dplyr::all_of(sel_column_id))

  return(res)
}
