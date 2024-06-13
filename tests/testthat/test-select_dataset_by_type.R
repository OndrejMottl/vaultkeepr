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
    select_dataset_by_type(sel_dataset_type = "vegetation_plot")

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
    select_dataset_by_type(sel_dataset_type = "vegetation_plot")

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("presence of only selected type", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(sel_dataset_type = "vegetation_plot")

  test_vec_dataset_type <-
    test_datasets$data %>%
    dplyr::distinct(dataset_type) %>%
    dplyr::collect() %>%
    dplyr::pull("dataset_type")


  testthat::expect_equal(length(test_vec_dataset_type), 1)
  testthat::expect_equal(test_vec_dataset_type, "vegetation_plot")
})

testthat::test_that("selecting two types", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(
      sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive")
    )

  test_vec_dataset_type <-
    test_datasets$data %>%
    dplyr::distinct(dataset_type) %>%
    dplyr::collect() %>%
    dplyr::pull("dataset_type")


  testthat::expect_equal(length(test_vec_dataset_type), 2)
  testthat::expect_equal(
    test_vec_dataset_type,
    c("vegetation_plot", "fossil_pollen_archive")
  )
})

testthat::test_that("size of a dataset", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(sel_dataset_type = "vegetation_plot")

  test_n_datasets <-
    test_datasets$data %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  testthat::expect_equal(test_n_datasets, 162)
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_dataset_by_type(sel_dataset_type = "vegetation_plot")
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
      select_dataset_by_type(sel_dataset_type = "vegetation_plot")
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
      select_dataset_by_type(sel_dataset_type = 123)
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
      select_dataset_by_type(sel_dataset_type = "bad_dataset_type")
  )
})
