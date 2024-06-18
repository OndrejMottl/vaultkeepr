#' @title Get taxa information
#' @description For each Sample, get information about Taxa and their abundances
#' from the Vault database
#' @param con A connection to the Vault database
#' @param classify_to A character vector specifying the taxonomic level to classify
#' the taxa to. The options are `original`, `species`, `genus`, and `family`.
#' @return A `vault_pipe` object with the data and the connection to the Vault database
#' @export
get_taxa <- function(
    con = NULL,
    classify_to = c("original", "species", "genus", "family")) {
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
    "SampleTaxa" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "SampleTaxa table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "SampleTaxa")),
    msg = paste(
      "The SampleTaxa does not contain `sample_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "SampleTaxa")),
    msg = paste(
      "The SampleTaxa does not contain `taxon_id` column in the Vault database.",
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

  data_taxa <-
    dplyr::tbl(sel_con, "SampleTaxa")

  data_taxa_classified <-
    classify_taxa(
      data_source = data_taxa,
      sel_con = sel_con,
      to = classify_to
    )

  # test for presence of trait values and output a warning
  #  that the column is going to be renamed
  if (
    c("taxon_id") %in% colnames(sel_data)
  ) {
    message(
      paste(
        "The column `taxon_id` is already present in the data,",
        "possibly from Trait data.",
        "Therefore, the column `taxon_id` from the `SampleTaxa` table",
        "is going to be renamed to `taxon_id_vegetation`",
        "to avoid any conflict.", "\n",
        "We recommned using `get_taxa()` before `get_traits()`"
      )
    )
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
