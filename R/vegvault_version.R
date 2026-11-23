is_vegvault_v2 <- function(db_con) {
  version <-
    load_vegvault_version(db_con = db_con)
  tryCatch(
    base::numeric_version(version) >= base::numeric_version("2.0.0"),
    error = function(error) FALSE
  )
}

get_trait_values_by_version <- function(db_con) {
  data_values <- dplyr::tbl(db_con, "TraitsValue")

  if (!is_vegvault_v2(db_con)) {
    return(data_values)
  }

  vec_required_tables <-
    c("TraitUnitConversion", "TraitDerivation", "Traits", "TraitsDomain")
  if (!all(vec_required_tables %in% DBI::dbListTables(db_con))) {
    cli::cli_abort("VegVault 2 trait provenance tables are incomplete.")
  }

  data_conversion <-
    dplyr::tbl(db_con, "TraitUnitConversion") %>%
    dplyr::select("trait_unit_conversion_id", "conversion_rule_key")
  data_derivation <-
    dplyr::tbl(db_con, "TraitDerivation") %>%
    dplyr::select("trait_derivation_id", "derivation_rule_key")
  data_units <-
    dplyr::tbl(db_con, "Traits") %>%
    dplyr::select("trait_id", "trait_domain_id") %>%
    dplyr::inner_join(
      dplyr::tbl(db_con, "TraitsDomain") %>%
        dplyr::select("trait_domain_id", "trait_unit"),
      by = "trait_domain_id"
    ) %>%
    dplyr::select(-"trait_domain_id")

  data_values %>%
    dplyr::left_join(data_conversion, by = "trait_unit_conversion_id") %>%
    dplyr::left_join(data_derivation, by = "trait_derivation_id") %>%
    dplyr::left_join(data_units, by = "trait_id")
}
