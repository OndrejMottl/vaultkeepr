#' @title Classify taxa
#' @description Classify taxa in a SQL table to a specific level based on the
#' classification table in the Vault database.
#' @param data_source A SQL table that contains `taxon_id` column.
#' @param sel_con A connection to the Vault database.
#' @param to A character vector that specifies the classification level to
#' classify the taxa. The options are `original`, `species`, `genus`, and
#' `family`.
#' @return A SQL table with the classified taxa.
#' @keywords internal
classify_taxa <- function(data_source = NULL, sel_con = NULL, to = c("original", "species", "genus", "family")) {
  assertthat::assert_that(
    inherits(data_source, "tbl_sql"),
    msg = "data_source must be a class of `tbl_sql`"
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(data_source),
    msg = paste(
      "The data does not contain `taxon_id` column. Please add",
      "`add_taxa()` to the pipe before this function."
    )
  )

  to <- match.arg(to)

  assertthat::assert_that(
    is.character(to),
    msg = "`to` must be a character vector"
  )

  assertthat::assert_that(
    all(to %in% c("original", "species", "genus", "family")),
    msg = paste(
      "The `to` must be one of the following:",
      "`original`, `species`, `genus`, `family`"
    )
  )

  to_long <- switch(to,
    original = "taxon_id",
    species = "taxon_species",
    genus = "taxon_genus",
    family = "taxon_family",
  )

  if (
    to_long == "taxon_id"
  ) {
    return(data_source)
  }

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "sel_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "TaxonClassification" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "TaxonClassification table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "TaxonClassification")),
    msg = paste(
      "The TaxonClassification does not contain `taxon_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  data_class_sub <-
    dplyr::tbl(sel_con, "TaxonClassification") %>%
    dplyr::select(
      "taxon_id",
      dplyr::all_of(to_long)
    ) %>%
    dplyr::rename(
      taxon_id_new = !!to_long
    )

  data_res <-
    data_source %>%
    dplyr::left_join(
      data_class_sub,
      by = "taxon_id"
    ) %>%
    dplyr::select(
      -"taxon_id"
    ) %>%
    dplyr::rename(
      taxon_id = "taxon_id_new"
    )

  return(data_res)
}
