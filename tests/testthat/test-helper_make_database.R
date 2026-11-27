# Test database -----
testthat::test_that("the test database has the schema of the schema file", {
  con_expected <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_expected)
  )

  build_schema_fixture(
    db_con = con_expected,
    file_sql = "vegvault_schema_2.0.0.sql"
  )

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = base::file.path(base::tempdir(), "example.sqlite"),
      flags = RSQLite::SQLITE_RO
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  list_expected <-
    load_schema_description(db_con = con_expected)

  res <-
    load_schema_description(db_con = con_fixture)

  testthat::expect_equal(
    purrr::chuck(res, "columns"),
    purrr::chuck(list_expected, "columns")
  )
  testthat::expect_equal(
    purrr::chuck(res, "indexes"),
    purrr::chuck(list_expected, "indexes")
  )
})


testthat::test_that("the test database is VegVault 2.0.0", {
  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = base::file.path(base::tempdir(), "example.sqlite"),
      flags = RSQLite::SQLITE_RO
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  res <-
    load_vegvault_version(db_con = con_fixture)

  testthat::expect_equal(res, "2.0.0")
})

# Vignette database -----
testthat::test_that("the vignette database has the schema of the schema file", {
  path_vignettes <-
    testthat::test_path("..", "..", "vignettes")

  testthat::skip_if_not(
    base::dir.exists(path_vignettes),
    message = "The vignettes are not part of the package build"
  )

  env_vignette <-
    base::new.env(parent = base::globalenv())

  path_vignette_db <-
    base::source(
      file = base::file.path(path_vignettes, "helper_make_example_db.R"),
      local = env_vignette,
      chdir = TRUE
    ) |>
    purrr::chuck("value")

  con_expected <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_expected)
  )

  build_schema_fixture(
    db_con = con_expected,
    file_sql = "vegvault_schema_2.0.0.sql"
  )

  con_vignette <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_vignette_db,
      flags = RSQLite::SQLITE_RO
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_vignette)
  )

  list_expected <-
    load_schema_description(db_con = con_expected)

  res <-
    load_schema_description(db_con = con_vignette)

  testthat::expect_equal(
    purrr::chuck(res, "columns"),
    purrr::chuck(list_expected, "columns")
  )
  testthat::expect_equal(
    purrr::chuck(res, "indexes"),
    purrr::chuck(list_expected, "indexes")
  )
})

testthat::test_that("the two databases use the same helper functions", {
  path_vignettes <-
    testthat::test_path("..", "..", "vignettes")

  testthat::skip_if_not(
    base::dir.exists(path_vignettes),
    message = "The vignettes are not part of the package build"
  )

  vec_helper_files <-
    c(
      "helper_apply_fixture_seed.R",
      "helper_build_position_link_fixture.R",
      "helper_build_reference_fixture.R",
      "helper_build_trait_rule_fixture.R"
    )

  purrr::walk(
    .x = vec_helper_files,
    .f = ~ {
      path_vignette_helper <-
        base::file.path(path_vignettes, .x)

      testthat::expect_true(
        base::file.exists(path_vignette_helper),
        label = path_vignette_helper
      )

      testthat::expect_identical(
        base::readLines(con = path_vignette_helper, warn = FALSE),
        base::readLines(con = testthat::test_path(.x), warn = FALSE),
        label = .x
      )
    }
  )
})
