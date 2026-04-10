testthat::test_that("error if con is not a vault_pipe", {
  testthat::expect_error(
    explore_vault(con = "not_a_vault_pipe")
  )
})

testthat::test_that("error if con db_con is not SQLiteConnection", {
  fake_con <-
    structure(
      base::list(
        data = tibble::tibble(),
        db_con = base::list()
      ),
      class = c("list", "vault_pipe")
    )

  testthat::expect_error(
    explore_vault(con = fake_con)
  )
})

testthat::test_that("returns character type", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  res <-
    explore_vault(con = con_db)

  testthat::expect_type(res, "character")

  DBI::dbDisconnect(
    purrr::chuck(con_db, "db_con")
  )
})

testthat::test_that("returns key table names", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  res <-
    explore_vault(con = con_db)

  testthat::expect_true(
    base::all(
      c("Datasets", "Taxa", "Samples") %in% res
    )
  )

  DBI::dbDisconnect(
    purrr::chuck(con_db, "db_con")
  )
})

testthat::test_that("returns all expected table names", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  res <-
    explore_vault(con = con_db)

  testthat::expect_equal(
    res,
    c(
      "AbioticData", "AbioticDataReference",
      "AbioticVariable", "AbioticVariableReference",
      "Authors", "DatasetReferences",
      "DatasetSample", "DatasetSourceTypeID",
      "DatasetSourceTypeReference", "DatasetSourcesID",
      "DatasetSourcesReference", "DatasetTypeID",
      "Datasets", "References", "SampleReference",
      "SampleSizeID", "SampleTaxa", "SampleUncertainty",
      "Samples", "SamplingMethodID",
      "SamplingMethodReference", "Taxa",
      "TaxonClassification", "TaxonReference",
      "Traits", "TraitsDomain", "TraitsReference",
      "TraitsValue", "sqlite_stat1", "sqlite_stat4",
      "version_control"
    )
  )

  DBI::dbDisconnect(
    purrr::chuck(con_db, "db_con")
  )
})
