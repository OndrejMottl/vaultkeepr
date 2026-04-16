#' @title Only select taxa by ID
#' @description Filter the Samples in dataset by taxon id
#' @param con A connection to the Vault database
#' @param sel_id A integer vector of taxa ids
#' @return A vault_pipe object
#' @export
select_taxa_by_id <- function(con = NULL, sel_id = NULL) {
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
    is.numeric(sel_id),
    msg = "{.arg sel_id} must be a numeric vector"
  )

  assertthat_cli(
    all(round(sel_id) == sel_id),
    msg = "{.arg sel_id} must be an integer vector"
  )

  assertthat_cli(
    length(sel_id) > 0,
    msg = "{.arg sel_id} must have at least one taxon id"
  )

  data_res <-
    sel_data %>%
    dplyr::filter(
      .data$taxon_id %in% sel_id
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
