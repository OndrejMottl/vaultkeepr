#' @title Select traits by domain name
#' @description Filters the data for the selected Trait Domain name.
#' @param con A connection object created by `open_vault()`.
#' @param sel_domain A character vector of the selected Trait Domain name.
#' @return A list of class `vault_pipe
#' @export
select_traits_by_domain_name <- function(
    con = NULL,
    sel_domain = NULL) {
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
    "trait_id" %in% colnames(sel_data),
    msg = "The data does not contain the {.code trait_id} column. Use {.fn get_traits} before this function"
  )

  assertthat_cli(
    "dataset_type" %in% colnames(sel_data),
    msg = "The data does not contain the {.code dataset_type} column. Use {.fn get_datasets} before this function"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  assertthat_cli(
    "TraitsDomain" %in% DBI::dbListTables(sel_con),
    msg = "The {.code TraitsDomain} table does not exist in the database. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "Traits" %in% DBI::dbListTables(sel_con),
    msg = "The {.code Traits} table does not exist in the database. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "trait_domain_id" %in% colnames(dplyr::tbl(sel_con, "TraitsDomain")),
    msg = "The {.code TraitsDomain} table does not contain the {.code trait_domain_id} column. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "trait_domain_id" %in% colnames(dplyr::tbl(sel_con, "Traits")),
    msg = "The {.code Traits} table does not contain the {.code trait_domain_id} column. Make sure to connect to the correct database"
  )

  assertthat_cli(
    is.character(sel_domain),
    msg = "{.arg sel_domain} must be a character vector"
  )

  data_traits <-
    dplyr::inner_join(
      dplyr::tbl(sel_con, "TraitsDomain"),
      dplyr::tbl(sel_con, "Traits"),
      by = "trait_domain_id"
    )

  dat_res <-
    sel_data %>%
    dplyr::left_join(
      data_traits,
      by = "trait_id"
    ) %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        .default = TRUE,
        !(.data$trait_domain_name %in% sel_domain) &
          .data$dataset_type == "traits" ~ FALSE
      )
    ) %>%
    dplyr::filter(
      .data$keep == TRUE
    ) %>%
    dplyr::select(-"keep")

  res <-
    structure(
      list(
        data = dat_res,
        db_con = sel_con
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
