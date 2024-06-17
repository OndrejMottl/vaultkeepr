#' @title Only select taxa by name
#' @description Filter the Samples in dataset by taxa name
#' @param con A connection to the Vault database
#' @param sel_taxa A character vector of taxa names
#' @return A vault_pipe object
#' @export
select_taxa_by_name <- function(con = NULL, sel_taxa = NULL) {
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
    "taxon_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `taxon_id` columns. Please add",
      "`get_taxa()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "Taxa" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "Taxa table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "Taxa")),
    msg = paste(
      "The Taxa does not contain `taxon_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    is.character(sel_taxa),
    msg = "`sel_taxa` must be a character vector"
  )

  assertthat::assert_that(
    length(sel_taxa) > 0,
    msg = "`sel_taxa` must have at least one taxon name"
  )

  data_res <-
    sel_data %>%
    dplyr::left_join(
      dplyr::tbl(sel_con, "Taxa"),
      by = "taxon_id"
    ) %>%
    dplyr::filter(
      .data$taxon_name %in% sel_taxa
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
