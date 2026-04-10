#' @title Classify taxa
#' @description Classify taxa in a SQL table to a specific level based on
#' the classification table in the Vault database, or a user-supplied
#' override table.
#' @param data_source A SQL table that contains `taxon_id` column.
#' @param sel_con A connection to the Vault database. Required when
#' `classification_data` is `NULL`.
#' @param to A character vector that specifies the classification level to
#' classify the taxa. The options are `original`, `species`, `genus`, and
#' `family`.
#' @param classification_data
#' An optional `data.frame` (or `tibble`) to use instead of the
#' `TaxonClassification` table from the database. Must contain a
#' `taxon_id` column and the column corresponding to `to` (i.e.
#' `taxon_species`, `taxon_genus`, or `taxon_family`). If `NULL`
#' (default), the classification is read from the database.
#' Obtain a valid table via
#' `get_classification_table(con, return_raw_data = TRUE)`.
#' @return A SQL table with the classified taxa.
#' @keywords internal
classify_taxa <- function(
    data_source = NULL,
    sel_con = NULL,
    to = c("original", "species", "genus", "family"),
    classification_data = NULL) {
  assertthat_cli(
    inherits(data_source, "tbl_sql"),
    msg = "{.arg data_source} must be a {.cls tbl_sql}"
  )

  assertthat_cli(
    "taxon_id" %in% colnames(data_source),
    msg = "The data does not contain the {.code taxon_id} column. Use {.fn get_taxa} before this function"
  )

  to <- match.arg(to)

  assertthat_cli(
    is.character(to),
    msg = "{.arg to} must be a character vector"
  )

  assertthat_cli(
    all(to %in% c("original", "species", "genus", "family")),
    msg = "{.arg to} must be one of: {.code original}, {.code species}, {.code genus}, {.code family}"
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

  if (
    !is.null(classification_data)
  ) {
    assertthat_cli(
      is.data.frame(classification_data),
      msg = "{.arg classification_data} must be a {.cls data.frame} or {.cls tibble}. Obtain one via {.code get_classification_table(con, return_raw_data = TRUE)}"
    )

    assertthat_cli(
      "taxon_id" %in% colnames(classification_data),
      msg = "{.arg classification_data} must contain a {.code taxon_id} column. Obtain a valid table via {.code get_classification_table(con, return_raw_data = TRUE)}"
    )

    assertthat_cli(
      to_long %in% colnames(classification_data),
      msg = stringr::str_glue(
        "{.arg classification_data} must contain a {.code {to_long}} column for {.code to = '{to}'}"
      )
    )

    data_class_sub <-
      classification_data %>%
      dplyr::select(
        "taxon_id",
        dplyr::all_of(to_long)
      ) %>%
      dplyr::rename(
        taxon_id_new = !!to_long
      )

    data_res <-
      data_source %>%
      dplyr::inner_join(
        data_class_sub,
        by = "taxon_id",
        copy = TRUE
      ) %>%
      dplyr::select(
        -"taxon_id"
      ) %>%
      dplyr::rename(
        taxon_id = "taxon_id_new"
      )

    return(data_res)
  }

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.arg sel_con} must be a {.cls SQLiteConnection}"
  )

  assertthat_cli(
    "TaxonClassification" %in% DBI::dbListTables(sel_con),
    msg = "The {.code TaxonClassification} table does not exist in the database. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "TaxonClassification")),
    msg = "The {.code TaxonClassification} table does not contain the {.code taxon_id} column. Make sure to connect to the correct database"
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
    dplyr::inner_join(
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
