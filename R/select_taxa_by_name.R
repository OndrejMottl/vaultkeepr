#' @title Only select taxa by name
#' @description Filter the Samples in dataset by taxa name
#' @param con A connection to the Vault database
#' @param sel_taxa A character vector of taxa names
#' @return A vault_pipe object
#' @export
select_taxa_by_name <- function(con = NULL, sel_taxa = NULL) {
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

  assertthat_cli(
    is.character(sel_taxa),
    msg = "{.arg sel_taxa} must be a character vector"
  )

  assertthat_cli(
    length(sel_taxa) > 0,
    msg = "{.arg sel_taxa} must have at least one taxon name"
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
