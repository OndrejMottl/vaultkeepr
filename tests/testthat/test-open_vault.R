# test dumb ----
testthat::test_that("simple connect", {
  con <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  testthat::expect_s4_class(con, "SQLiteConnection")

  DBI::dbDisconnect(con)
})


# Test classes -----
testthat::test_that("return correct class-high", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_s3_class(con_db, "vault_pipe")

  DBI::dbDisconnect(con_db$db_con)
})


testthat::test_that("return correct class-low-connection", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_s4_class(con_db$db_con, "SQLiteConnection")

  DBI::dbDisconnect(con_db$db_con)
})

testthat::test_that("return correct class-low-data", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_s3_class(con_db$data, "tbl_df")

  DBI::dbDisconnect(con_db$db_con)
})


# Test tables -----
testthat::test_that("return corrent tables", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_equal(
    DBI::dbListTables(con_db$db_con),
    c(
      "AbioticData",
      "AbioticDataReference",
      "AbioticVariable",
      "AbioticVariableReference",
      "Authors",
      "DatasetReferences",
      "DatasetSample",
      "DatasetSourceTypeID",
      "DatasetSourceTypeReference",
      "DatasetSourcesID",
      "DatasetSourcesReference",
      "DatasetTypeID",
      "Datasets",
      "References",
      "SampleReference",
      "SampleSizeID",
      "SampleTaxa",
      "SampleUncertainty",
      "Samples",
      "SamplingMethodID",
      "SamplingMethodReference",
      "Taxa",
      "TaxonClassification",
      "TaxonReference",
      "Traits",
      "TraitsDomain",
      "TraitsReference",
      "TraitsValue",
      "sqlite_stat1",
      "sqlite_stat4",
      "version_control"
    )
  )

  DBI::dbDisconnect(con_db$db_con)
})


# Error handling -----

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = "not_a_file.sqlite",
      verbose = FALSE
    )
  )
})

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = 1,
      verbose = FALSE
    )
  )
})

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = 1
    )
  )
})

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = "true"
    )
  )
})

testthat::test_that("error handling - non-sqlite file", {
  path_non_sqlite <-
    base::tempfile(fileext = ".db")

  base::writeLines("not a sqlite file", path_non_sqlite)

  testthat::expect_error(
    open_vault(
      path = path_non_sqlite,
      verbose = FALSE
    )
  )

  base::unlink(path_non_sqlite)
})