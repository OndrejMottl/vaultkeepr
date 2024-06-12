testthat::test_that("basic data.frame structure", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets()

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("data.frame colnames", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    purrr::chuck("data")

  testthat::expect_equal(
    colnames(test_datasets),
    c(
      "dataset_id",
      "dataset_name",
      "data_source_id",
      "dataset_type_id",
      "data_source_type_id",
      "coord_long",
      "coord_lat",
      "sampling_method_id"
    )
  )
})

testthat::test_that("data.frame nrows", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    purrr::chuck("data")

  testthat::expect_equal(
    test_datasets %>%
      dplyr::collect() %>%
      nrow(),
    486
  )
})


testthat::test_that("data.frame values", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    purrr::chuck("data")

  data_temp <-
    tidyr::expand_grid(
      data_source_id = 1:3,
      dataset_type_id = 1:3,
      data_source_type_id = 1:3,
      sampling_method_id = 1:3,
      tibble::tribble(
        ~coord_long, ~coord_lat,
        -115, 45,
        15, 45,
        115, 45,
        -60, -15,
        -15, -30,
        -135, -30
      )
    ) %>%
    dplyr::mutate(,
      dataset_name = paste0("dataset_", dplyr::row_number())
    ) %>%
    dplyr::select(
      colnames(test_datasets)[-1]
    )

  testthat::expect_equal(
    dplyr::collect(test_datasets)[, -1],
    data_temp
  )
})