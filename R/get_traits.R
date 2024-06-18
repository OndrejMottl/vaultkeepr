#' @title Get trait values for the data
#' @description Get trait values for the data from the Vault database
#' @param con A connection object created using `open_vault()`
#' @param classify_to A character vector specifying the taxonomic level
#' to classify the taxa to. Default is `original`
#' @param verbose A logical value indicating whether to print messages.
#' Default is `TRUE`
#' @return A connection object with the data and database connection
#' @export
#' @details
#' If the function is used after `get_taxa()`, the trait values will be
#' returned only for the taxa present in the data. If you prefer to
#' return all trait values, we recommend using `get_traits()` before
#' `get_taxa()` in the pipe.
#' In addition, it is important to set `classify_to` to the same value as
#' in `get_taxa()`.
get_traits <- function(
    con = NULL,
    classify_to = c("original", "species", "genus", "family"),
    verbose = TRUE) {
  .data <- rlang::.data

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
    "dataset_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `dataset_id` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `sample_id` columns. Please add",
      "`get_samples()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "TraitsValue" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "TraitsValue table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "TraitsValue")),
    msg = paste(
      "The TraitsValue does not contain `sample_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "TraitsValue")),
    msg = paste(
      "The TraitsValue does not contain `taxon_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  classify_to <- match.arg(classify_to)

  assertthat::assert_that(
    is.character(classify_to),
    msg = "`classify_to` must be a character vector"
  )

  assertthat::assert_that(
    all(classify_to %in% c("original", "species", "genus", "family")),
    msg = paste(
      "The `classify_to` must be one of the following:",
      "`original`, `species`, `genus`, `family`"
    )
  )

  assertthat::assert_that(
    inherits(verbose, "logical"),
    msg = "`verbose` must be a logical value"
  )


  data_traits <-
    classify_taxa(
      data_source = dplyr::tbl(sel_con, "TraitsValue"),
      sel_con = sel_con,
      to = classify_to
    )

  # test for presence of taxa values and output a warning
  #  that the data will be subset
  if (
    c("taxon_id") %in% colnames(sel_data)
  ) {
    if (
      isTRUE(verbose)
    ) {
      message(
        paste(
          "The column `taxon_id` is already present in the data.",
          "Therefore, trait values will be returned only for the taxa present",
          "in the data. If you prefer to return all trait values,",
          " we recommned using `get_traits()` before `get_taxa()` in the pipe."
        )
      )

      message(
        paste(
          "! Important !: ",
          "Make sure to set `classify_to` to same value as in `get_taxa()`"
        )
      )

      message(
        paste(
          "In addition, the column `taxon_id`",
          "is going to be renamed to `taxon_id_trait`",
          "to avoid any conflict.", "\n"
        )
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
