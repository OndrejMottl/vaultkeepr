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
    get_abiotic_data(verbose = FALSE) %>%
    select_abiotic_var_by_id(1)

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
    get_abiotic_data(verbose = FALSE) %>%
    select_abiotic_var_by_id(1)

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("get only correct variable", {
  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(verbose = FALSE) %>%
    select_abiotic_var_by_id(1) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(abiotic_variable_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    purrr::chuck("abiotic_variable_id")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 1)
  testthat::expect_equal(test_datasets_values, 1)
})

testthat::test_that("get correct taxa for multi-taxa selection", {
  test_vec_var_id <- 2:3

  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(verbose = FALSE) %>%
    select_abiotic_var_by_id(
      sel_var_id = test_vec_var_id
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(abiotic_variable_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    purrr::chuck("abiotic_variable_id")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 2)
  testthat::expect_equal(test_datasets_values, test_vec_var_id)
})

testthat::test_that("subset dataset", {
  test_datasets_full <-
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

  test_datasets_sub <-
    test_datasets_full %>%
    select_abiotic_var_by_id(1)

  nrows_full <-
    test_datasets_full$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  nrows_sub <-
    test_datasets_sub$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_true(is.numeric(nrows_full))
  testthat::expect_true(is.numeric(nrows_sub))
  testthat::expect_true(nrows_full > 0)
  testthat::expect_true(nrows_sub > 0)
  testthat::expect_true(nrows_sub < nrows_full)
})

testthat::test_that("return empty dataset", {
  nrows_empty <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(verbose = FALSE) %>%
    # select a variable that does not exist
    select_abiotic_var_by_id(Inf) %>%
    purrr::chuck("data") %>%
    dplyr::filter(dataset_type == "gridpoints") %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_equal(nrows_empty, 0)
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_abiotic_var_by_id(1)
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
      select_abiotic_var_by_id(1)
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
      select_abiotic_var_by_id(1)
  )
})

testthat::test_that("error wihtout `get_abiotic_data()`", {
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
      select_abiotic_var_by_id(1)
  )
})

testthat::test_that("error with bad `sel_var_id` class", {
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
      get_abiotic_data(verbose = FALSE) %>%
      select_abiotic_var_by_id(
        sel_var_id = "temperature"
      )
  )
})
