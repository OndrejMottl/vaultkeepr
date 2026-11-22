build_expected_classified_values <- function(
  data_selected,
  data_values,
  data_classification,
  sel_rank_column,
  vec_join_column,
  sel_suffix
) {
  # The documented classification: each taxon id is replaced by the id at
  #   the chosen rank, and taxa missing from the classification are dropped.
  #   Joins never match missing keys, as in SQL.
  if (
    sel_rank_column == "taxon_id"
  ) {
    data_values_classified <- data_values
  } else {
    data_classification_rank <-
      data_classification |>
      dplyr::select(
        dplyr::all_of(c("taxon_id", sel_rank_column))
      ) |>
      dplyr::rename(
        taxon_id_new = dplyr::all_of(sel_rank_column)
      )

    data_values_classified <-
      data_values |>
      dplyr::inner_join(
        y = data_classification_rank,
        by = dplyr::join_by("taxon_id"),
        na_matches = "never"
      ) |>
      dplyr::select(-"taxon_id") |>
      dplyr::rename(taxon_id = "taxon_id_new")
  }

  res <-
    data_selected |>
    dplyr::left_join(
      y = data_values_classified,
      by = vec_join_column,
      suffix = c("", sel_suffix),
      na_matches = "never"
    )

  return(res)
}
