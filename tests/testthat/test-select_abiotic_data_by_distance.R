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
    select_abiotic_data_by_distance(
      sel_km_distance = 50,
      verbose = FALSE
    )

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
    select_abiotic_data_by_distance(
      sel_km_distance = 50,
      verbose = FALSE
    )

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("size of a total dataset", {
  test_data_full <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets()

  test_data_sub <-
    test_data_full %>%
    select_abiotic_data_by_distance(
      sel_km_distance = 50,
      verbose = FALSE
    )

  test_n_data_full <-
    test_data_full %>%
    purrr::chuck("data") %>%
    dplyr::filter(dataset_type == "gridpoints") %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  test_n_data_sub <-
    test_data_sub %>%
    purrr::chuck("data") %>%
    dplyr::filter(dataset_type == "gridpoints") %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  testthat::expect_true(is.numeric(test_n_data_full))
  testthat::expect_true(is.numeric(test_n_data_sub))
  testthat::expect_true(test_n_data_sub <= test_n_data_full)
  # comment out as the whole function will be recoded
  #testthat::expect_equal(test_n_data_full, 162)
})

testthat::test_that("get a message when sel_dataset_type differ", {
  test_f <- function() {
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_abiotic_data_by_distance(
        sel_km_distance = 50,
        verbose = TRUE
      )
  }

  quiet_f <-
    purrr::quietly(test_f)

  res <-
    quiet_f()

  testthat::expect_true(length(res$messages) > 0)
})


# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_abiotic_data_by_distance(
      sel_km_distance = 50,
      verbose = FALSE
    )
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
      select_abiotic_data_by_distance(
        sel_km_distance = 50,
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `sel_dataset_type` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_abiotic_data_by_distance(
        sel_km_distance = 50,
        sel_dataset_type = 123,
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `sel_dataset_type`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_abiotic_data_by_distance(
        sel_km_distance = 50,
        sel_dataset_type = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `sel_km_distance` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_abiotic_data_by_distance(
        sel_km_distance = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `sel_degree_grid ` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_abiotic_data_by_distance(
        sel_km_distance = 50,
        sel_degree_grid = "bad",
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
      select_abiotic_data_by_distance(
        sel_km_distance = 50,
        verbose = 123
      )
  )
})
