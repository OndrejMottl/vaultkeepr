testthat::test_that("return correct class", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(verbose = FALSE)

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
    extract_data(verbose = FALSE)

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
    extract_data(verbose = FALSE)

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

  testthat::expect_equal(
    test_datasets$dataset_name %>% unique() %>% length(),
    actual_datasets$dataset_name %>% unique() %>% length()
  )

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
    extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )

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
    extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )

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
    extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )

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
    extract_data(verbose = FALSE)
  )
})

testthat::test_that("error with bad `con` class", {
  testthat::expect_error(
    extract_data(
      con = 123,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `con`", {
  testthat::expect_error(
    extract_data(
      con = "bad",
      verbose = FALSE
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
      return_raw_data = "bad",
      verbose = FALSE
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
      return_raw_data = 123,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `check_mandatory_references`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      check_mandatory_references = "bad",
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `check_mandatory_references`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      check_mandatory_references = 123,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `verbose`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      verbose = "bad"
    )
  )
})

testthat::test_that("error with bad `verbose`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      verbose = 123
    )
  )
})
