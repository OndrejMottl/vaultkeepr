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
    select_dataset_by_type() %>%
    get_samples() %>%
    select_samples_by_age(verbose = FALSE)

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
    select_dataset_by_type() %>%
    get_samples() %>%
    select_samples_by_age(verbose = FALSE)

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("basic age filter", {
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
      sel_dataset_type = "vegetation_plot"
    ) %>%
    get_samples() %>%
    select_samples_by_age(
      sel_dataset_type = "vegetation_plot",
      age_lim = c(0, 5000),
      verbose = FALSE
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(age) %>%
    dplyr::collect() %>%
    purrr::chuck("age")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values >= 0))
  testthat::expect_true(all(test_datasets_values <= 5000))
})

testthat::test_that("only filter by sel_dataset_type", {
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
    ) %>%
    get_samples() %>%
    select_samples_by_age(
      sel_dataset_type = "vegetation_plot",
      age_lim = c(0, 5000),
      verbose = FALSE
    )

  data_filter_vegetation <-
    test_datasets$data %>%
    dplyr::filter(dataset_type == "vegetation_plot") %>%
    dplyr::select(age) %>%
    dplyr::collect() %>%
    unlist()

  data_filter_fossil <-
    test_datasets$data %>%
    dplyr::filter(dataset_type == "fossil_pollen_archive") %>%
    dplyr::select(age) %>%
    dplyr::collect() %>%
    unlist()

  testthat::expect_true(all(data_filter_vegetation <= 5000))
  testthat::expect_false(all(data_filter_fossil <= 5000))
})

testthat::test_that("get a message when sel_dataset_type differ", {
  testthat::expect_message(
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
      ) %>%
      get_samples() %>%
      select_samples_by_age(
        sel_dataset_type = "gridpoints",
        age_lim = c(0, 5000),
        verbose = TRUE
      )
  )
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_samples_by_age()
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
      select_samples_by_age()
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
      select_samples_by_age()
  )
})

testthat::test_that("error wihtout `select_dataset_by_type()`", {
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
      select_samples_by_age()
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
      select_dataset_by_type() %>%
      get_samples() %>%
      select_samples_by_age(
        sel_dataset_type = 123,
        verbose = FALSE
      )
  )
})


testthat::test_that("error with bad `age_lim` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_type() %>%
      get_samples() %>%
      select_samples_by_age(
        age_lim = "bad",
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
      select_dataset_by_type() %>%
      get_samples() %>%
      select_samples_by_age(
        verbose = 123
      )
  )
})
