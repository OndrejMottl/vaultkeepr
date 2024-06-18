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
    select_dataset_by_type("gridpoints") %>%
    get_samples() %>%
    get_abiotic() %>%
    select_abiotic_var_by_name("temperature")

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
    select_dataset_by_type("gridpoints") %>%
    get_samples() %>%
    get_abiotic() %>%
    select_abiotic_var_by_name("temperature")

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
    select_dataset_by_type("gridpoints") %>%
    get_samples() %>%
    get_abiotic() %>%
    select_abiotic_var_by_name("temperature") %>%
    purrr::chuck("data") %>%
    dplyr::distinct(abiotic_variable_name) %>%
    dplyr::collect() %>%
    purrr::chuck("abiotic_variable_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 1)
  testthat::expect_equal(test_datasets_values, "temperature")
})

testthat::test_that("get correct taxa for multi-taxa selection", {
  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("gridpoints") %>%
    get_samples() %>%
    get_abiotic() %>%
    select_abiotic_var_by_name(
      sel_var_name = c("temperature", "precipitation")
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(abiotic_variable_name) %>%
    dplyr::collect() %>%
    purrr::chuck("abiotic_variable_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 2)
  testthat::expect_equal(
    test_datasets_values,
    c("temperature", "precipitation")
  )
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
    select_dataset_by_type("gridpoints") %>%
    get_samples() %>%
    get_abiotic()

  test_datasets_sub <-
    test_datasets_full %>%
    select_abiotic_var_by_name("temperature")

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
    select_dataset_by_type("gridpoints") %>%
    get_samples() %>%
    get_abiotic() %>%
    # select a variable that does not exist
    select_abiotic_var_by_name("pikachu") %>%
    purrr::chuck("data") %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_equal(nrows_empty, 0)
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_abiotic_var_by_name("temperature")
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
      select_abiotic_var_by_name("temperature")
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
      select_dataset_by_type("gridpoints") %>%
      select_abiotic_var_by_name("temperature")
  )
})

testthat::test_that("error wihtout `get_abiotic()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_type("gridpoints") %>%
      get_samples() %>%
      select_abiotic_var_by_name("temperature")
  )
})

testthat::test_that("error with bad `sel_var_name` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_type("gridpoints") %>%
      get_samples() %>%
      get_abiotic() %>%
      select_abiotic_var_by_name(
        sel_var_name = 123
      )
  )
})
