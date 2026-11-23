# Inputs -----
testthat::test_that("is_vegvault_v2() rejects a non-connection", {
  testthat::expect_error(
    base::suppressMessages(
      is_vegvault_v2(db_con = "not a connection")
    )
  )
})


# Output -----
testthat::test_that("is_vegvault_v2() returns one logical value", {
  path_fixture <-
    build_version_table_fixture(vec_version = c("1.0.0", "2.0.0"))

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  res <-
    is_vegvault_v2(db_con = con_fixture)

  testthat::expect_type(res, "logical")
  testthat::expect_length(res, 1L)
})


# Functionality -----
testthat::test_that("is_vegvault_v2() is TRUE for VegVault 2", {
  list_version <-
    list(
      "2.0.0" = c("1.0.0", "2.0.0"),
      "2.10.0" = c("2.0.0", "2.9.0", "2.10.0")
    )

  purrr::iwalk(
    .x = list_version,
    .f = ~ {
      path_fixture <-
        build_version_table_fixture(
          vec_version = .x,
          sel_layout = "2.0.0"
        )

      con_fixture <-
        DBI::dbConnect(
          drv = RSQLite::SQLite(),
          dbname = path_fixture
        )

      res <-
        is_vegvault_v2(db_con = con_fixture)

      DBI::dbDisconnect(conn = con_fixture)

      testthat::expect_true(res, label = .y)
    }
  )
})

testthat::test_that("is_vegvault_v2() is FALSE for VegVault 1", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c("0.0.2", "0.0.3", "0.0.4", "1.0.0"),
      sel_layout = "1.0.0"
    )

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  res <-
    is_vegvault_v2(db_con = con_fixture)

  testthat::expect_false(res)
})


# Edge cases -----
testthat::test_that("is_vegvault_v2() is FALSE without version rows", {
  path_fixture <-
    build_version_table_fixture(vec_version = NULL)

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  res <-
    is_vegvault_v2(db_con = con_fixture)

  testthat::expect_false(res)
})

testthat::test_that("is_vegvault_v2() is FALSE for an unreadable version", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c("1.0.0", "2.0.0-dev"),
      sel_layout = "2.0.0"
    )

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  res <-
    is_vegvault_v2(db_con = con_fixture)

  testthat::expect_false(res)
})
