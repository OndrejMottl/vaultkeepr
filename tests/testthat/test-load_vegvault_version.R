# Inputs -----
testthat::test_that("load_vegvault_version() rejects a non-connection", {
  testthat::expect_error(
    base::suppressMessages(
      load_vegvault_version(db_con = "not a connection")
    )
  )

  testthat::expect_message(
    base::try(
      load_vegvault_version(db_con = "not a connection"),
      silent = TRUE
    ),
    regexp = "valid database connection"
  )
})

testthat::test_that("load_vegvault_version() rejects a closed connection", {
  path_fixture <-
    build_version_table_fixture(vec_version = "2.0.0")

  con_closed <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  DBI::dbDisconnect(conn = con_closed)

  testthat::expect_error(
    base::suppressMessages(
      load_vegvault_version(db_con = con_closed)
    )
  )
})


# Output -----
testthat::test_that("load_vegvault_version() returns one version string", {
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
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_equal(res, "2.0.0")
})


# The row with the highest id -----
testthat::test_that("load_vegvault_version() reads the highest id", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c(
        "0.0.2", "0.0.3", "0.0.4", "1.0.0", "2.0.0", "2.9.0", "2.10.0"
      ),
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
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_equal(res, "2.10.0")
})

testthat::test_that("load_vegvault_version() lets the id decide", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c("2.0.0", "1.0.0"),
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
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_equal(res, "1.0.0")
})

testthat::test_that("load_vegvault_version() reads the 1.0.0 layout", {
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
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_equal(res, "1.0.0")
})


# Edge cases -----
testthat::test_that("load_vegvault_version() skips rows without a version", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c("1.0.0", NA_character_),
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
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_equal(res, "1.0.0")
})

testthat::test_that("load_vegvault_version() reads no version rows as 1.0.0", {
  list_fixture <-
    list(
      no_table = build_version_table_fixture(vec_version = NULL),
      empty_2 = build_version_table_fixture(
        vec_version = character(),
        sel_layout = "2.0.0"
      ),
      empty_1 = build_version_table_fixture(
        vec_version = character(),
        sel_layout = "1.0.0"
      )
    )

  purrr::iwalk(
    .x = list_fixture,
    .f = ~ {
      con_fixture <-
        DBI::dbConnect(
          drv = RSQLite::SQLite(),
          dbname = .x
        )

      withr::defer(
        DBI::dbDisconnect(conn = con_fixture)
      )

      res <-
        load_vegvault_version(db_con = con_fixture)

      testthat::expect_equal(res, "1.0.0", label = .y)
    }
  )
})

testthat::test_that("load_vegvault_version() leaves the connection open", {
  path_fixture <-
    build_version_table_fixture(vec_version = "2.0.0")

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  res <-
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_equal(res, "2.0.0")
  testthat::expect_true(DBI::dbIsValid(con_fixture))
})
