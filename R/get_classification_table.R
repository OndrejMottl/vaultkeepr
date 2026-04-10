#' @title Get the classification table for the data
#' @description
#' Extract the `TaxonClassification` table for all taxa present in the
#' current dataset. The result is a collected `tibble` (not a `vault_pipe`)
#' and is intended as a "pipeline split": the user can inspect, edit, and
#' pass the result back into [get_taxa()] or [get_traits()] via their
#' `classification_data` argument.
#' @param con
#' A `vault_pipe` object created by [open_vault()]. Must already contain
#' `taxon_id` in the data, i.e. [get_taxa()] with
#' `classify_to = "original"` must have been called earlier in the pipe.
#' @param return_raw_data
#' A `logical`. If `FALSE` (default), returns a `tibble` with both IDs
#' and resolved names: `taxon_id`, `taxon_name`, `taxon_species`,
#' `species_name`, `taxon_genus`, `genus_name`, `taxon_family`, and
#' `family_name`. This format is human-readable and can also be fed
#' back as `classification_data` in [get_taxa()] or [get_traits()].
#' If `TRUE`, returns a raw `tibble` with only ID columns `taxon_id`,
#' `taxon_species`, `taxon_genus`, and `taxon_family` matching the
#' `TaxonClassification` schema.
#' @return
#' When `return_raw_data = FALSE` (default): a `tibble` with eight
#' columns — `taxon_id`, `taxon_name`, `taxon_species`, `species_name`,
#' `taxon_genus`, `genus_name`, `taxon_family`, `family_name` —
#' restricted to taxa present in the data. Both IDs and resolved names
#' are included, so the tibble can be inspected directly and also passed
#' to the `classification_data` argument of [get_taxa()] or [get_traits()].
#' When `return_raw_data = TRUE`: a `tibble` with columns `taxon_id`,
#' `taxon_species`, `taxon_genus`, and `taxon_family` restricted to
#' the species-level taxon IDs present in the data.
#' @details
#' This function must be called after [get_taxa()] with
#' `classify_to = "original"` so that the data contains species-level
#' `taxon_id` values that match the `TaxonClassification` table.
#' Calling it after a higher-level classification (e.g. `"genus"`) will
#' yield an empty result because the genus IDs are not primary keys in
#' `TaxonClassification`.
#'
#' Typical workflow:
#' ```r
#' con_taxa <-
#'   open_vault(path) |>
#'   get_datasets() |>
#'   get_samples() |>
#'   get_taxa(classify_to = "original")
#'
#' data_class <-
#'   get_classification_table(con_taxa, return_raw_data = TRUE)
#'
#' # inspect / edit data_class, then feed back:
#' con_taxa |>
#'   get_taxa(
#'     classify_to = "genus",
#'     classification_data = data_class
#'   )
#' ```
#' @seealso [open_vault()], [get_taxa()], [get_traits()]
#' @export
get_classification_table <- function(
    con = NULL,
    return_raw_data = FALSE) {
  assertthat::assert_that(
    inherits(con, "vault_pipe"),
    msg = paste(
      "`con` must be a class of `vault_pipe`",
      "Use `open_vault()` to create a connection"
    )
  )

  assertthat::assert_that(
    all(names(con) %in% c("data", "db_con")),
    msg = paste(
      "con must have `data` and `db_con`",
      "Use `open_vault()` to create a connection"
    )
  )

  sel_data <- con$data

  assertthat::assert_that(
    inherits(sel_data, "tbl"),
    msg = "data must be a class of `tbl`"
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(sel_data),
    msg = paste(
      "The data does not contain `taxon_id` column.",
      "Please add `get_taxa(classify_to = 'original')`",
      "to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    is.logical(return_raw_data),
    msg = "The 'return_raw_data' must be a logical"
  )

  assertthat::assert_that(
    "TaxonClassification" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "TaxonClassification table does not exist in the Vault",
      "database. Make sure to connect to the correct database."
    )
  )

  assertthat::assert_that(
    "Taxa" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "Taxa table does not exist in the Vault database.",
      "Make sure to connect to the correct database."
    )
  )

  data_taxon_ids <-
    sel_data %>%
    dplyr::select("taxon_id") %>%
    dplyr::distinct()

  data_class_raw <-
    dplyr::tbl(sel_con, "TaxonClassification") %>%
    dplyr::semi_join(
      data_taxon_ids,
      by = "taxon_id"
    )

  if (
    isTRUE(return_raw_data)
  ) {
    res <-
      dplyr::collect(data_class_raw)

    return(res)
  }

  data_taxa_names <-
    dplyr::tbl(sel_con, "Taxa") %>%
    dplyr::select("taxon_id", "taxon_name")

  res <-
    data_class_raw %>%
    dplyr::left_join(
      data_taxa_names,
      by = "taxon_id"
    ) %>%
    dplyr::left_join(
      data_taxa_names %>%
        dplyr::rename(
          taxon_species = "taxon_id",
          species_name = "taxon_name"
        ),
      by = "taxon_species"
    ) %>%
    dplyr::left_join(
      data_taxa_names %>%
        dplyr::rename(
          taxon_genus = "taxon_id",
          genus_name = "taxon_name"
        ),
      by = "taxon_genus"
    ) %>%
    dplyr::left_join(
      data_taxa_names %>%
        dplyr::rename(
          taxon_family = "taxon_id",
          family_name = "taxon_name"
        ),
      by = "taxon_family"
    ) %>%
    dplyr::select(
      "taxon_id",
      "taxon_name",
      "taxon_species",
      "species_name",
      "taxon_genus",
      "genus_name",
      "taxon_family",
      "family_name"
    ) %>%
    dplyr::collect()

  return(res)
}
