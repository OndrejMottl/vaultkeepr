# Build a small SQLite database with one trait value, in the layout of
#   VegVault 1.0.0 or 2.0.0, from the schema files shipped with the package.
#   The 2.0.0 database has the unit of the trait domain ("mm2"), one
#   unit-conversion rule ("identity-mm2") and one derivation rule
#   ("sla-to-lma"), both used by the value. Returns the path of the file;
#   the caller removes it.
build_trait_version_fixture <- function(sel_version = c("1.0.0", "2.0.0")) {
  sel_version_matched <-
    base::match.arg(sel_version)

  flag_version_2 <-
    sel_version_matched == "2.0.0"

  path_fixture <-
    base::tempfile(fileext = ".sqlite")

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  build_schema_fixture(
    db_con = con_fixture,
    file_sql = stringr::str_c(
      "vegvault_schema_",
      sel_version_matched,
      ".sql"
    )
  )

  vec_statements_common <-
    c(
      "INSERT INTO version_control (version) VALUES (?)",
      "INSERT INTO Traits (trait_id, trait_domain_id, trait_name)
        VALUES (1, 1, 'leaf area')"
    )

  vec_statements_version <-
    if (
      flag_version_2
    ) {
      c(
        "INSERT INTO TraitsDomain (trait_domain_id, trait_domain_name,
          trait_unit) VALUES (1, 'Leaf Area', 'mm2')",
        "INSERT INTO TraitUnitConversion (trait_unit_conversion_id,
          conversion_rule_key, rule_version, source_provider,
          source_trait_id, normalized_original_unit, standardized_unit,
          trait_dimension, operation, multiplier, evidence)
          VALUES (1, 'identity-mm2', 'fixture', 'fixture', '1', 'mm2',
          'mm2', 'area', 'identity', 1, 'test fixture')",
        "INSERT INTO TraitDerivation (trait_derivation_id,
          derivation_rule_key, rule_version, input_unit, output_unit,
          trait_dimension, operation, multiplier, evidence)
          VALUES (1, 'sla-to-lma', 'fixture', 'mm2/mg', 'g/m2',
          'mass per area', 'reciprocal', 1000, 'test fixture')",
        "INSERT INTO TraitsValue (trait_id, dataset_id, sample_id, taxon_id,
          trait_value, trait_unit_conversion_id, trait_derivation_id)
          VALUES (1, 1, 1, 1, 10, 1, 1)"
      )
    } else {
      c(
        "INSERT INTO TraitsDomain (trait_domain_id, trait_domain_name)
          VALUES (1, 'Leaf Area')",
        "INSERT INTO TraitsValue (trait_id, dataset_id, sample_id, taxon_id,
          trait_value) VALUES (1, 1, 1, 1, 10)"
      )
    }

  DBI::dbExecute(
    conn = con_fixture,
    statement = purrr::chuck(vec_statements_common, 1),
    params = list(sel_version_matched)
  )

  purrr::walk(
    .x = c(
      purrr::chuck(vec_statements_common, 2),
      vec_statements_version
    ),
    .f = ~ DBI::dbExecute(
      conn = con_fixture,
      statement = .x
    )
  )

  res <-
    path_fixture

  return(res)
}
