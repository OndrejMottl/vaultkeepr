#' @title Get taxa information
#' @description For each Sample, get information about Taxa and their
#' abundances from the Vault database.
#' @param con A connection to the Vault database.
#' @param classify_to
#' A character vector specifying the taxonomic level to classify the taxa
#' to. The options are `original`, `species`, `genus`, and `family`.
#' @param classification_data
#' An optional `data.frame` (or `tibble`) used instead of the
#' `TaxonClassification` table in the database. Must contain `taxon_id`
#' and the column matching `classify_to` (e.g. `taxon_genus` when
#' `classify_to = "genus"`). If `NULL` (default), the database table
#' is used. Obtain a valid table via
#' `get_classification_table(con, return_raw_data = TRUE)`.
#' @param verbose A logical value indicating whether to print messages.
#' Default is `TRUE`.
#' @return
#' A `vault_pipe` object with the data and the connection to the Vault
#' database.
#' @export
get_taxa <- function(
    con = NULL,
    classify_to = c("original", "species", "genus", "family"),
    classification_data = NULL,
    verbose = TRUE) {
  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a class of {.cls vault_pipe}. Use {.fn open_vault} to create a connection."
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con}. Use {.fn open_vault} to create a connection."
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code data} must be a class of {.cls tbl}."
  )

  assertthat_cli(
    "sample_id" %in% colnames(sel_data),
    msg = "The dataset does not contain {.code sample_id} column. Please add {.fn get_samples} to the pipe before this function."
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code db_con} must be a class of {.cls SQLiteConnection}."
  )

  assertthat_cli(
    "SampleTaxa" %in% DBI::dbListTables(sel_con),
    msg = "{.code SampleTaxa} table does not exist in the Vault database. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "SampleTaxa")),
    msg = "The {.code SampleTaxa} table does not contain {.code sample_id} column. Make sure to connect to the correct database."
  )

  assertthat_cli(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "SampleTaxa")),
    msg = "The {.code SampleTaxa} table does not contain {.code taxon_id} column. Make sure to connect to the correct database."
  )

  classify_to <- match.arg(classify_to)

  assertthat_cli(
    is.character(classify_to),
    msg = "{.arg classify_to} must be a character vector."
  )

  assertthat_cli(
    all(classify_to %in% c("original", "species", "genus", "family")),
    msg = "{.arg classify_to} must be one of {.code original}, {.code species}, {.code genus}, or {.code family}."
  )

  if (
    !is.null(classification_data)
  ) {
    assertthat_cli(
      is.data.frame(classification_data),
      msg = "{.arg classification_data} must be a {.cls data.frame} or {.cls tibble}. Obtain one via {.code get_classification_table(con, return_raw_data = TRUE)}"
    )
  }

  data_taxa <-
    dplyr::tbl(sel_con, "SampleTaxa")

  data_taxa_classified <-
    classify_taxa(
      data_source = data_taxa,
      sel_con = sel_con,
      to = classify_to,
      classification_data = classification_data
    )

  # test for presence of trait values and output a warning
  #  that the column is going to be renamed
  if (
    c("taxon_id") %in% colnames(sel_data)
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_alert_warning(
        stringr::str_c(
          "The column {.code taxon_id} is already present in the data, possibly from Trait data. ",
          "Therefore, the column {.code taxon_id} from the {.code SampleTaxa} table ",
          "is going to be renamed to {.code taxon_id_vegetation} to avoid any conflict. ",
          "We recommend using {.fn get_taxa} before {.fn get_traits}."
        )
      )
    }
  }

  data_res <-
    sel_data %>%
    dplyr::left_join(
      data_taxa_classified,
      by = "sample_id",
      suffix = c("", "_vegetation")
    )

  res <-
    structure(
      list(
        data = data_res,
        db_con = sel_con
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
