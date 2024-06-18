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
    "trait_id" %in% colnames(sel_data),
    msg = paste(
      "The data does not contain `trait_id` columns. Please add",
      "`get_traits()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "dataset_type" %in% colnames(sel_data),
    msg = paste(
      "The data should be filtered only for `traits` dataset type.",
      "However, the does not contain `dataset_type` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "TraitsDomain" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "TraitsDomain table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "Traits" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "Traits table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "trait_domain_id" %in% colnames(dplyr::tbl(sel_con, "TraitsDomain")),
    msg = paste(
      "The TraitsDomain does not contain `trait_domain_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "trait_domain_id" %in% colnames(dplyr::tbl(sel_con, "Traits")),
    msg = paste(
      "The Traits does not contain `trait_domain_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    is.character(sel_domain),
    msg = "`sel_var_name` must be a character vector"
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
