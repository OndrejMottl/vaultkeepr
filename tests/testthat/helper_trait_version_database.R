make_trait_version_database <- function(version = c("1.0.0", "2.0.0")) {
  selected_version <- match.arg(version)
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  DBI::dbExecute(
    con,
    "CREATE TABLE version_control (id INTEGER PRIMARY KEY, version TEXT)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO version_control (version) VALUES (?)",
    params = list(selected_version)
  )
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE TraitsDomain (trait_domain_id INTEGER PRIMARY KEY,",
      "trait_domain_name TEXT, trait_unit TEXT)"
    )
  )
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE Traits (trait_id INTEGER PRIMARY KEY,",
      "trait_domain_id INTEGER, trait_name TEXT)"
    )
  )

  if (selected_version == "1.0.0") {
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE TraitsValue (trait_id INTEGER, dataset_id INTEGER,",
        "sample_id INTEGER, taxon_id INTEGER, trait_value REAL)"
      )
    )
  } else {
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE TraitUnitConversion (trait_unit_conversion_id",
        "INTEGER PRIMARY KEY, conversion_rule_key TEXT)"
      )
    )
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE TraitDerivation (trait_derivation_id INTEGER",
        "PRIMARY KEY, derivation_rule_key TEXT)"
      )
    )
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE TraitsValue (trait_id INTEGER, dataset_id INTEGER,",
        "sample_id INTEGER, taxon_id INTEGER, trait_value REAL,",
        "trait_unit_conversion_id INTEGER, trait_derivation_id INTEGER)"
      )
    )
    DBI::dbExecute(
      con,
      "INSERT INTO TraitUnitConversion VALUES (1, 'identity-mm2')"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO TraitDerivation VALUES (1, 'sla-to-lma')"
    )
  }

  DBI::dbExecute(con, "INSERT INTO TraitsDomain VALUES (1, 'Leaf Area', 'mm2')")
  DBI::dbExecute(con, "INSERT INTO Traits VALUES (1, 1, 'leaf area')")
  if (selected_version == "1.0.0") {
    DBI::dbExecute(con, "INSERT INTO TraitsValue VALUES (1, 1, 1, 1, 10)")
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO TraitsValue VALUES (1, 1, 1, 1, 10, 1, 1)"
    )
  }
  DBI::dbDisconnect(con)
  path
}
