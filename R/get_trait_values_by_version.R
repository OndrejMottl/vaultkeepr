#' @title Get the trait values of a database with their units
#' @description
#' Build a lazy table of the trait values of a VegVault database. For
#' VegVault 2.0.0 or newer, it adds the unit of each trait and the keys of the
#' unit-conversion and derivation rules of each value.
#' @param db_con
#' A valid `DBI` connection to a VegVault SQLite database.
#' @return
#' A lazy table (`tbl_sql`) of `TraitsValue`. For VegVault 2.0.0 or newer it
#' has the extra columns `conversion_rule_key`, `derivation_rule_key` and
#' `trait_unit`; for VegVault 1 it has the columns of the table as stored.
#' @details
#' The version is read by [is_vegvault_v2()]. For VegVault 2.0.0 or newer,
#' the tables `TraitUnitConversion`, `TraitDerivation`, `Traits` and
#' `TraitsDomain` must exist, otherwise the function stops with an error. The
#' rule keys and units are added with left joins, so a value without a
#' conversion or derivation rule is kept, with a missing key.
#' @seealso [is_vegvault_v2()], [get_traits()]
#' @keywords internal
get_trait_values_by_version <- function(db_con) {
  data_values <-
    dplyr::tbl(
      src = db_con,
      from = "TraitsValue"
    )

  if (
    !is_vegvault_v2(db_con = db_con)
  ) {
    return(data_values)
  }

  vec_required_tables <-
    c("TraitUnitConversion", "TraitDerivation", "Traits", "TraitsDomain")

  if (
    !all(vec_required_tables %in% DBI::dbListTables(conn = db_con))
  ) {
    cli::cli_abort("VegVault 2 trait provenance tables are incomplete.")
  }

  data_conversion <-
    dplyr::tbl(
      src = db_con,
      from = "TraitUnitConversion"
    ) %>%
    dplyr::select("trait_unit_conversion_id", "conversion_rule_key")

  data_derivation <-
    dplyr::tbl(
      src = db_con,
      from = "TraitDerivation"
    ) %>%
    dplyr::select("trait_derivation_id", "derivation_rule_key")

  data_units <-
    dplyr::tbl(
      src = db_con,
      from = "Traits"
    ) %>%
    dplyr::select("trait_id", "trait_domain_id") %>%
    dplyr::inner_join(
      dplyr::tbl(
        src = db_con,
        from = "TraitsDomain"
      ) %>%
        dplyr::select("trait_domain_id", "trait_unit"),
      by = "trait_domain_id"
    ) %>%
    dplyr::select(-"trait_domain_id")

  res <-
    data_values %>%
    dplyr::left_join(data_conversion, by = "trait_unit_conversion_id") %>%
    dplyr::left_join(data_derivation, by = "trait_derivation_id") %>%
    dplyr::left_join(data_units, by = "trait_id")

  return(res)
}
