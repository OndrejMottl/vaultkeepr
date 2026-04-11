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

  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a {.cls vault_pipe} object. Use {.fn open_vault} to create a connection"
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con} elements. Use {.fn open_vault} to create a connection"
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code con$data} must be a {.cls tbl}"
  )

  assertthat_cli(
    "taxon_id" %in% colnames(sel_data),
    msg = "The data does not contain the {.code taxon_id} column. Use {.fn get_taxa} before this function"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  assertthat_cli(
    "Taxa" %in% DBI::dbListTables(sel_con),
    msg = "The {.code Taxa} table does not exist in the database. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "taxon_id" %in% colnames(dplyr::tbl(sel_con, "Taxa")),
    msg = "The {.code Taxa} table does not contain the {.code taxon_id} column. Make sure to connect to the correct database"
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
