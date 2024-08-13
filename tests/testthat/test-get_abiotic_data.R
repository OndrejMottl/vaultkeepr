testthat::test_that("return correct class-high", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data()

  testthat::expect_s3_class(test_datasets, "vault_pipe")
})

testthat::test_that("basic data.frame structure", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data()

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("number of abiotic variables", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data()

  test_n_abiotic_variables <-
    test_datasets$data %>%
    dplyr::distinct(abiotic_variable_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_n_abiotic_variables <-
    dplyr::tbl(con_db, "AbioticData") %>%
    dplyr::distinct(abiotic_variable_id) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  testthat::expect_equal(
    test_n_abiotic_variables,
    actual_n_abiotic_variables
  )

  DBI::dbDisconnect(con_db)
})

testthat::test_that("abiotic data only preset for dataset type 4", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data()

  test_abiotic_variables_na <-
    test_datasets$data %>%
    dplyr::filter(dataset_type_id %in% c(1:3)) %>%
    dplyr::distinct(abiotic_variable_id) %>%
    dplyr::collect() %>%
    dplyr::pull("abiotic_variable_id")

  testthat::expect_true(length(test_abiotic_variables_na) == 1)
  testthat::expect_true(is.na(test_abiotic_variables_na))
})

testthat::test_that("size of a total dataset", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data()

  test_n_datasets <-
    test_datasets$data %>%
    #dplyr::filter(!is.na(abiotic_variable_id)) %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_n_datasets <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "AbioticData"),
      by = "sample_id"
    ) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  testthat::expect_equal(test_n_datasets, actual_n_datasets)

  DBI::dbDisconnect(con_db)
})

# errors -----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    get_abiotic_data()
  )
})

testthat::test_that("error wihtout `get_datasets()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_abiotic_data()
  )
})

testthat::test_that("error wihtout `get_samples()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_abiotic_data()
  )
})
