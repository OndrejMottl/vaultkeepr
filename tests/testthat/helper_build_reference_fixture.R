# Draw dummy references for the ids of one table of the test or vignette
#   database, append them to `References` and write the table that links
#   them to the ids. The same file is in tests/testthat/ and vignettes/;
#   test-helper_make_database.R checks that the two copies match.
build_reference_fixture <- function(
  con_db,
  id_table_name,
  ref_table_name,
  name_column_id,
  dummy_ref_name,
  mandatory = FALSE,
  n_refs = 5
) {
  data_ids <-
    dplyr::tbl(
      src = con_db,
      from = id_table_name
    ) %>%
    dplyr::distinct(!!dplyr::sym(name_column_id)) %>%
    dplyr::collect()

  apply_fixture_seed()

  # The magrittr pipe is kept, because `nrow(.)` needs its placeholder
  data_references <-
    data_ids %>%
    dplyr::mutate(
      reference_dummy = base::sample(
        x = c(NA_integer_, 1:n_refs),
        size = base::nrow(.),
        replace = TRUE
      )
    ) %>%
    dplyr::filter(!base::is.na(.data[["reference_dummy"]])) %>%
    dplyr::mutate(
      reference_detail = stringr::str_c(
        dummy_ref_name,
        "_",
        .data[["reference_dummy"]]
      ),
      mandatory = mandatory
    )

  data_references_distinct <-
    data_references %>%
    dplyr::distinct(
      .data[["reference_detail"]],
      .data[["mandatory"]]
    ) %>%
    dplyr::arrange(.data[["reference_detail"]])

  dplyr::copy_to(
    dest = con_db,
    df = data_references_distinct,
    name = "References",
    append = TRUE
  )

  data_references_db <-
    dplyr::tbl(
      src = con_db,
      from = "References"
    ) %>%
    dplyr::collect()

  data_reference_links <-
    data_references %>%
    dplyr::left_join(
      data_references_db,
      by = "reference_detail"
    ) %>%
    dplyr::distinct(
      !!dplyr::sym(name_column_id),
      .data[["reference_id"]]
    )

  res <-
    dplyr::copy_to(
      dest = con_db,
      df = data_reference_links,
      name = ref_table_name,
      append = TRUE
    )

  return(base::invisible(res))
}
