#' @title Get taxon names from a vault plan
#' @description
#' Retrieve a tibble of distinct taxon names from the current `vault_pipe`
#' query plan without executing a full `extract_data()` call. Requires
#' `get_taxa()` to have been called earlier in the pipe so that `taxon_id`
#' is present in the data.
#' @param con
#' A `vault_pipe` object. Must have been passed through `get_taxa()` so
#' that a `taxon_id` column is present in `con$data`.
#' @return
#' A `tibble` with a single column `taxon_name` containing the distinct,
#' alphabetically sorted taxon names available in the current query plan.
#' @seealso [get_taxa()], [select_taxa_by_name()]
#' @export
get_taxon_names <- function(con = NULL) {
  .data <- rlang::.data

  assertthat::assert_that(
    inherits(con, "vault_pipe"),
    msg = paste(
      "`con` must be a class of `vault_pipe`",
      "Use `open_vault()` to create a connection"
    )
  )

  assertthat::assert_that(
    base::all(base::names(con) %in% c("data", "db_con")),
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
      "The dataset does not contain `taxon_id` column. Please add",
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
      "Taxa table does not exist in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "Taxa")),
    msg = paste(
      "The Taxa table does not contain `taxon_id` column.",
      "Make sure to connect to the correct database"
    )
  )

  res <-
    sel_data %>%
    dplyr::filter(!is.na(.data$taxon_id)) %>%
    dplyr::distinct(.data$taxon_id) %>%
    dplyr::left_join(
      dplyr::tbl(sel_con, "Taxa"),
      by = "taxon_id"
    ) %>%
    dplyr::distinct(.data$taxon_name) %>%
    dplyr::filter(!is.na(.data$taxon_name)) %>%
    dplyr::arrange(.data$taxon_name) %>%
    dplyr::collect()

  return(res)
}
