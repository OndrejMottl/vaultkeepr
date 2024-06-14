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

  testthat::expect_equal(nrow(test_datasets), 648)
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
