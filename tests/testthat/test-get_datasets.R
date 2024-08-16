testthat::test_that("return correct class-high", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets()

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
      "sampling_method_id",
      "dataset_type"
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

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )
  actual_datasets <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::collect()

  testthat::expect_equal(
    test_datasets %>%
      dplyr::collect() %>%
      nrow(),
    nrow(actual_datasets)
  )

  DBI::dbDisconnect(con_db)
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

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_datasets <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetTypeID"),
      by = "dataset_type_id"
    ) %>%
    dplyr::collect()

  testthat::expect_equal(
    dplyr::collect(test_datasets)[, -1],
    actual_datasets[, -1]
  )
})
