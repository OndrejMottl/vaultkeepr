#' @title Get trait values for the data
#' @description Get trait values for the data from the Vault database.
#' @param con A connection object created using [open_vault()].
#' @param classify_to
#' A character vector specifying the taxonomic level to classify the
#' taxa to. Default is `original`.
#' @param verbose
#' A `logical` indicating whether to print messages. Default is `TRUE`.
#' @param classification_data
#' An optional `data.frame` (or `tibble`) used instead of the
#' `TaxonClassification` table in the database. Must contain `taxon_id`
#' and the column matching `classify_to` (e.g. `taxon_genus` when
#' `classify_to = "genus"`). If `NULL` (default), the database table
#' is used. Obtain a valid table via
#' `get_classification_table(con, return_raw_data = TRUE)`.
#' @return A connection object with the data and database connection.
#' @export
#' @details
#' If the function is used after [get_taxa()], the trait values will be
#' returned only for the taxa present in the data. If you prefer to
#' return all trait values, we recommend using [get_traits()] before
#' [get_taxa()] in the pipe.
#' In addition, it is important to set `classify_to` to the same value
#' as in [get_taxa()].
get_traits <- function(
    con = NULL,
    classify_to = c("original", "species", "genus", "family"),
    verbose = TRUE,
    classification_data = NULL) {
  .data <- rlang::.data

  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a class of {.cls vault_pipe}. Use {.fn open_vault} to create a connection.",
    verbose = verbose
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con}. Use {.fn open_vault} to create a connection.",
    verbose = verbose
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code data} must be a class of {.cls tbl}.",
    verbose = verbose
  )

  assertthat_cli(
    "dataset_id" %in% colnames(sel_data),
    msg = "The dataset does not contain {.code dataset_id} column. Please add {.fn get_datasets} to the pipe before this function.",
    verbose = verbose
  )

  assertthat_cli(
    "sample_id" %in% colnames(sel_data),
    msg = "The dataset does not contain {.code sample_id} column. Please add {.fn get_samples} to the pipe before this function.",
    verbose = verbose
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code db_con} must be a class of {.cls SQLiteConnection}.",
    verbose = verbose
  )

  assertthat_cli(
    "TraitsValue" %in% DBI::dbListTables(sel_con),
    msg = "{.code TraitsValue} table does not exist in the Vault database. Make sure to connect to the correct database.",
    verbose = verbose
  )

  assertthat_cli(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "TraitsValue")),
    msg = "The {.code TraitsValue} table does not contain {.code sample_id} column. Make sure to connect to the correct database.",
    verbose = verbose
  )

  assertthat_cli(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "TraitsValue")),
    msg = "The {.code TraitsValue} table does not contain {.code taxon_id} column. Make sure to connect to the correct database.",
    verbose = verbose
  )

  classify_to <- match.arg(classify_to)

  assertthat_cli(
    is.character(classify_to),
    msg = "{.arg classify_to} must be a character vector.",
    verbose = verbose
  )

  assertthat_cli(
    all(classify_to %in% c("original", "species", "genus", "family")),
    msg = "{.arg classify_to} must be one of {.code original}, {.code species}, {.code genus}, or {.code family}.",
    verbose = verbose
  )

  assertthat_cli(
    inherits(verbose, "logical"),
    msg = "{.arg verbose} must be a logical value."
  )

  if (
    !is.null(classification_data)
  ) {
    assertthat_cli(
      is.data.frame(classification_data),
      msg = "{.arg classification_data} must be a {.cls data.frame} or {.cls tibble}. Obtain one via {.code get_classification_table(con, return_raw_data = TRUE)}",
      verbose = verbose
    )
  }

  data_traits <-
    classify_taxa(
      data_source = dplyr::tbl(sel_con, "TraitsValue"),
      sel_con = sel_con,
      to = classify_to,
      classification_data = classification_data
    )

  # test for presence of taxa values and output a warning
  #  that the data will be subset
  if (
    c("taxon_id") %in% colnames(sel_data)
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_alert_warning(
        stringr::str_c(
          "The column {.code taxon_id} is already present in the data. ",
          "Therefore, trait values will be returned only for the taxa present in the data. ",
          "If you prefer to return all trait values, we recommend using {.fn get_traits} before {.fn get_taxa} in the pipe."
        )
      )

      cli::cli_alert_warning(
        "Make sure to set {.arg classify_to} to the same value as in {.fn get_taxa}."
      )

      cli::cli_alert_warning(
        "In addition, the column {.code taxon_id} is going to be renamed to {.code taxon_id_trait} to avoid any conflict."
      )
    }

    data_taxa <-
      sel_data %>%
      dplyr::distinct(.data$taxon_id)

    data_res <-
      dplyr::left_join(
        sel_data,
        dplyr::inner_join(
          data_traits,
          data_taxa,
          by = "taxon_id"
        ),
        by = c("dataset_id", "sample_id"),
        suffix = c("", "_trait")
      )
  } else {
    data_res <-
      sel_data %>%
      dplyr::left_join(
        data_traits,
        by = c("dataset_id", "sample_id"),
        suffix = c("", "_trait")
      )
  }

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
