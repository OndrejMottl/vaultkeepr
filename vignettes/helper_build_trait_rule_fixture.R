# Write one unit-conversion rule per trait domain into `TraitUnitConversion`
#   of the test or vignette database, as VegVault 2.0.0 requires a rule for
#   every trait value. Each rule keeps the unit of its domain (`TraitsDomain`
#   must be filled first), and its id equals the id of the domain, so a
#   trait value takes the rule of the domain of its trait. `TraitDerivation`
#   stays empty: no fixture value is derived from another trait.
#   The same file is in tests/testthat/ and vignettes/;
#   test-helper_make_database.R checks that the two copies match.
build_trait_rule_fixture <- function(con_db) {
  data_trait_domains <-
    dplyr::tbl(
      src = con_db,
      from = "TraitsDomain"
    ) |>
    dplyr::select("trait_domain_id", "trait_unit") |>
    dplyr::collect()

  data_conversion_rules <-
    data_trait_domains |>
    dplyr::mutate(
      trait_unit_conversion_id = .data[["trait_domain_id"]],
      conversion_rule_key = stringr::str_c(
        "fixture-identity-",
        .data[["trait_domain_id"]]
      ),
      rule_version = "fixture",
      source_provider = "fixture",
      source_trait_id = base::as.character(.data[["trait_domain_id"]]),
      source_dataset_key = NA_character_,
      normalized_original_unit = .data[["trait_unit"]],
      standardized_unit = .data[["trait_unit"]],
      trait_dimension = "fixture",
      operation = "identity",
      multiplier = 1,
      evidence = "test fixture"
    ) |>
    dplyr::select(-"trait_domain_id", -"trait_unit")

  dplyr::copy_to(
    dest = con_db,
    df = data_conversion_rules,
    name = "TraitUnitConversion",
    append = TRUE
  )

  res <-
    data_conversion_rules

  return(base::invisible(res))
}
