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
    get_abiotic_data(verbose = FALSE)

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
    get_abiotic_data(verbose = FALSE)

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
    get_abiotic_data(verbose = FALSE)

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
    get_abiotic_data(verbose = FALSE)

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
    get_abiotic_data(verbose = FALSE)

  test_n_datasets <-
    test_datasets$data %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::select(-"sample_id_link") %>%
    dplyr::distinct() %>%
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

  testthat::expect_true(test_n_datasets <= actual_n_datasets)

  DBI::dbDisconnect(con_db)
})

testthat::test_that("gridpoint data have `sample_id_link", {
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
    get_abiotic_data(verbose = FALSE)

  test_data <-
    test_datasets$data %>%
    dplyr::filter(dataset_type_id == 4)

  test_vec_sample_id_link <-
    test_data %>%
    dplyr::pull("sample_id_link")


  testthat::expect_false(
    any(is.na(test_vec_sample_id_link))
  )
  testthat::expect_true(
    length(test_vec_sample_id_link) > 0
  )
  testthat::expect_true(
    all(is.integer(test_vec_sample_id_link))
  )
  testthat::expect_true(
    all(test_vec_sample_id_link > 0)
  )
})

testthat::test_that("gridpoint data have `abiotic_variable_id", {
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
    get_abiotic_data(verbose = FALSE)

  test_data <-
    test_datasets$data %>%
    dplyr::filter(dataset_type_id == 4)

  test_vec_abiotic_variable_id <-
    test_data %>%
    dplyr::pull("abiotic_variable_id")

  testthat::expect_false(
    any(is.na(test_vec_abiotic_variable_id))
  )
  testthat::expect_true(
    length(test_vec_abiotic_variable_id) > 0
  )
  testthat::expect_true(
    all(is.integer(test_vec_abiotic_variable_id))
  )
  testthat::expect_true(
    all(test_vec_abiotic_variable_id > 0)
  )
})

testthat::test_that("gridpoint data have `abiotic_value", {
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
    get_abiotic_data(verbose = FALSE)

  test_data <-
    test_datasets$data %>%
    dplyr::filter(dataset_type_id == 4)

  test_vec_value <-
    test_data %>%
    dplyr::pull("abiotic_value")

  testthat::expect_false(
    any(is.na(test_vec_value))
  )
  testthat::expect_true(
    length(test_vec_value) > 0
  )
  testthat::expect_true(
    all(is.numeric(test_vec_value))
  )
})

testthat::test_that("`mode` does not change the size of dataset", {
  test_n_datasets_mean <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(
      mode = "mean",
      verbose = FALSE
    ) %>%
    purrr::chuck("data") %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  test_n_datasets_median <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(
      mode = "median",
      verbose = FALSE
    ) %>%
    purrr::chuck("data") %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  test_n_datasets_nearest <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(
      mode = "nearest",
      verbose = FALSE
    ) %>%
    purrr::chuck("data") %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  testthat::expect_true(
    test_n_datasets_mean == test_n_datasets_median
  )
  testthat::expect_true(
    test_n_datasets_mean == test_n_datasets_nearest
  )
  testthat::expect_true(
    test_n_datasets_median == test_n_datasets_nearest
  )
})

# errors -----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    get_abiotic_data(verbose = FALSE)
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
      get_abiotic_data(verbose = FALSE)
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
      get_abiotic_data(verbose = FALSE)
  )
})

testthat::test_that("error with bad `mode` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_abiotic_data(
        mode = 123,
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `mode` value", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_abiotic_data(
        mode = "bad",
        verbose = FALSE
      )
  )
})


testthat::test_that("error with bad `limit_by_distance_km` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_abiotic_data(
        limit_by_distance_km = "bad",
        verbose = FALSE
      )
  )
})


testthat::test_that("error with bad `limit_by_age_years` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_abiotic_data(
        limit_by_age_years = "bad",
        verbose = FALSE
      )
  )
})


testthat::test_that("error with bad `verbose` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_abiotic_data(
        verbose = 123
      )
  )
})
