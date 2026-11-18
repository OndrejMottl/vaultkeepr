testthat::test_that("open_vault detects versions without changing its shape", {
  path_v1_shape <- make_trait_version_database("1.0.0")
  path_v2_shape <- make_trait_version_database("2.0.0")
  withr::defer(unlink(c(path_v1_shape, path_v2_shape)))

  vault_v1 <- open_vault(path_v1_shape, verbose = FALSE)
  vault_v2 <- open_vault(path_v2_shape, verbose = FALSE)
  withr::defer(DBI::dbDisconnect(vault_v1$db_con))
  withr::defer(DBI::dbDisconnect(vault_v2$db_con))

  testthat::expect_named(vault_v1, c("data", "db_con"))
  testthat::expect_identical(attr(vault_v1, "database_version"), "1.0.0")
  testthat::expect_identical(attr(vault_v2, "database_version"), "2.0.0")
})

testthat::test_that("v1 traits remain lazy and v2 adds unit provenance", {
  path_v1_provenance <- make_trait_version_database("1.0.0")
  path_v2_provenance <- make_trait_version_database("2.0.0")
  withr::defer(unlink(c(path_v1_provenance, path_v2_provenance)))
  con_v1 <- DBI::dbConnect(RSQLite::SQLite(), path_v1_provenance)
  con_v2 <- DBI::dbConnect(RSQLite::SQLite(), path_v2_provenance)
  withr::defer(DBI::dbDisconnect(con_v1))
  withr::defer(DBI::dbDisconnect(con_v2))

  data_v1 <- get_trait_values_by_version(con_v1)
  data_v2 <- get_trait_values_by_version(con_v2)
  testthat::expect_s3_class(data_v1, "tbl_sql")
  testthat::expect_s3_class(data_v2, "tbl_sql")
  testthat::expect_false("trait_unit" %in% colnames(data_v1))
  testthat::expect_true(
    all(c("trait_unit", "conversion_rule_key", "derivation_rule_key") %in%
      colnames(data_v2))
  )
  observed <- dplyr::collect(data_v2)
  testthat::expect_identical(observed$trait_unit, "mm2")
  testthat::expect_identical(observed$conversion_rule_key, "identity-mm2")
})
