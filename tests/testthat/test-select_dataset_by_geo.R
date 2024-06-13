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
    select_dataset_by_geo(verbose = FALSE)

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
    select_dataset_by_geo(verbose = FALSE)

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("basic longitude filter", {
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
    select_dataset_by_geo(
      long_lim = c(0, 180),
      sel_dataset_type = "vegetation_plot"
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(coord_long) %>%
    dplyr::collect() %>%
    purrr::chuck("coord_long")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values >= 0))
  testthat::expect_true(all(test_datasets_values <= 180))
})

testthat::test_that("basic latitude filter", {
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
    select_dataset_by_geo(
      lat_lim = c(0, -90),
      sel_dataset_type = "vegetation_plot"
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(coord_lat) %>%
    dplyr::collect() %>%
    purrr::chuck("coord_lat")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values <= 0))
  testthat::expect_true(all(test_datasets_values >= -90))
})

testthat::test_that("basic lat/long filter", {
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
    select_dataset_by_geo(
      lat_lim = c(0, 90),
      long_lim = c(0, 90),
      sel_dataset_type = "vegetation_plot"
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(coord_lat, coord_long) %>%
    dplyr::collect() %>%
    unlist()

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values >= 0))
  testthat::expect_true(all(test_datasets_values <= 90))
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
    select_dataset_by_geo(
      lat_lim = c(0, 90),
      long_lim = c(0, 90),
      sel_dataset_type = "vegetation_plot",
      verbose = FALSE
    )

  data_filter_vegetation <-
    test_datasets$data %>%
    dplyr::filter(dataset_type == "vegetation_plot") %>%
    dplyr::select(coord_lat, coord_long) %>%
    dplyr::collect() %>%
    unlist()

  data_filter_fossil <-
    test_datasets$data %>%
    dplyr::filter(dataset_type == "fossil_pollen_archive") %>%
    dplyr::select(coord_lat, coord_long) %>%
    dplyr::collect() %>%
    unlist()

  testthat::expect_true(all(data_filter_vegetation >= 0))
  testthat::expect_true(all(data_filter_vegetation <= 90))

  testthat::expect_false(all(data_filter_fossil >= 0))
  testthat::expect_false(all(data_filter_fossil <= 90))
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
      select_dataset_by_geo(
        sel_dataset_type = "vegetation_plot",
        verbose = TRUE
      )
  )
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_dataset_by_geo()
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
      select_dataset_by_geo()
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
      select_dataset_by_geo()
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
      select_dataset_by_geo(
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
      select_dataset_by_type() %>%
      select_dataset_by_geo(
        sel_dataset_type = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `long_lim` class", {
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
      select_dataset_by_geo(
        long_lim = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `lat_lim` class", {
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
      select_dataset_by_geo(
        lat_lim = "bad",
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
      select_dataset_by_geo(
        verbose = 123
      )
  )
})
