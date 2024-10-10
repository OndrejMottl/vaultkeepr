testthat::test_that("return correct class", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data()

  testthat::expect_s3_class(test_datasets, "data.frame")
})

testthat::test_that("return empty data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data()

  testthat::expect_equal(nrow(test_datasets), 0)
})

testthat::test_that("return full data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data()

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

  testthat::expect_equal(nrow(test_datasets), nrow(actual_datasets))

  DBI::dbDisconnect(con_db)
})

testthat::test_that("return correct class", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(return_raw_data = TRUE)

  testthat::expect_s3_class(test_datasets, "data.frame")
})

testthat::test_that("return empty data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(return_raw_data = TRUE)

  testthat::expect_equal(nrow(test_datasets), 0)
})

testthat::test_that("return full data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data(return_raw_data = TRUE)

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

  testthat::expect_equal(nrow(test_datasets), nrow(actual_datasets))

  DBI::dbDisconnect(con_db)
})

# errors -----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    extract_data()
  )
})

testthat::test_that("error with bad `con` class", {
  testthat::expect_error(
    extract_data(con = 123)
  )
})

testthat::test_that("error with bad `con`", {
  testthat::expect_error(
    extract_data(con = "bad")
  )
})

testthat::test_that("error with bad `return_raw_data`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      return_raw_data = "bad"
    )
  )
})

testthat::test_that("error with bad `return_raw_data`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      return_raw_data = 123
    )
  )
})
