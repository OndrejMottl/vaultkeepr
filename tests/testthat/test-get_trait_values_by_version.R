# Inputs -----
testthat::test_that("get_trait_values_by_version() rejects a non-connection", {
  testthat::expect_error(
    base::suppressMessages(
      get_trait_values_by_version(db_con = "not a connection")
    )
  )
})


# Output -----
testthat::test_that("get_trait_values_by_version() returns a lazy table", {
  path_v1 <-
    build_trait_version_fixture(sel_version = "1.0.0")

  path_v2 <-
    build_trait_version_fixture(sel_version = "2.0.0")

  withr::defer(
    base::unlink(x = c(path_v1, path_v2))
  )

  con_v1 <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_v1
    )

  con_v2 <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_v2
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_v1)
  )

  withr::defer(
    DBI::dbDisconnect(conn = con_v2)
  )

  res_v1 <-
    get_trait_values_by_version(db_con = con_v1)

  res_v2 <-
    get_trait_values_by_version(db_con = con_v2)

  testthat::expect_s3_class(res_v1, "tbl_sql")
  testthat::expect_s3_class(res_v2, "tbl_sql")
})


# Functionality -----
testthat::test_that("get_trait_values_by_version() keeps VegVault 1 as is", {
  path_v1 <-
    build_trait_version_fixture(sel_version = "1.0.0")

  withr::defer(
    base::unlink(x = path_v1)
  )

  con_v1 <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_v1
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_v1)
  )

  res <-
    get_trait_values_by_version(db_con = con_v1)

  testthat::expect_equal(
    base::colnames(res),
    c("trait_id", "dataset_id", "sample_id", "taxon_id", "trait_value")
  )
})

testthat::test_that("get_trait_values_by_version() adds VegVault 2 units", {
  path_v2 <-
    build_trait_version_fixture(sel_version = "2.0.0")

  withr::defer(
    base::unlink(x = path_v2)
  )

  con_v2 <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_v2
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_v2)
  )

  data_values <-
    get_trait_values_by_version(db_con = con_v2) |>
    dplyr::collect()

  testthat::expect_equal(base::nrow(data_values), 1L)
  testthat::expect_equal(
    dplyr::pull(data_values, "trait_unit"),
    "mm2"
  )
  testthat::expect_equal(
    dplyr::pull(data_values, "conversion_rule_key"),
    "identity-mm2"
  )
  testthat::expect_equal(
    dplyr::pull(data_values, "derivation_rule_key"),
    "sla-to-lma"
  )
})


# Edge cases -----
testthat::test_that("get_trait_values_by_version() keeps underived values", {
  path_v2 <-
    build_trait_version_fixture(sel_version = "2.0.0")

  withr::defer(
    base::unlink(x = path_v2)
  )

  con_v2 <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_v2
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_v2)
  )

  DBI::dbExecute(
    conn = con_v2,
    statement = stringr::str_c(
      "INSERT INTO TraitsValue (trait_id, dataset_id, sample_id, taxon_id, ",
      "trait_value, trait_unit_conversion_id, trait_derivation_id) ",
      "VALUES (1, 1, 2, 1, 20, 1, NULL)"
    )
  )

  data_values <-
    get_trait_values_by_version(db_con = con_v2) |>
    dplyr::filter(.data[["sample_id"]] == 2L) |>
    dplyr::collect()

  testthat::expect_equal(base::nrow(data_values), 1L)
  testthat::expect_equal(
    dplyr::pull(data_values, "trait_unit"),
    "mm2"
  )
  testthat::expect_equal(
    dplyr::pull(data_values, "conversion_rule_key"),
    "identity-mm2"
  )
  testthat::expect_true(
    base::is.na(dplyr::pull(data_values, "derivation_rule_key"))
  )
})

testthat::test_that("get_trait_values_by_version() needs the rule tables", {
  path_v2 <-
    build_trait_version_fixture(sel_version = "2.0.0")

  withr::defer(
    base::unlink(x = path_v2)
  )

  con_v2 <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_v2
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_v2)
  )

  DBI::dbExecute(
    conn = con_v2,
    statement = "DROP TABLE TraitDerivation"
  )

  testthat::expect_error(
    get_trait_values_by_version(db_con = con_v2),
    regexp = "provenance tables are incomplete"
  )
})
