# test dumb ----
testthat::test_that("simple connect", {
  con <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  testthat::expect_s4_class(con, "SQLiteConnection")

  DBI::dbDisconnect(con)
})


# Test classes -----
testthat::test_that("return correct class-high", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_s3_class(con_db, "vault_pipe")

  DBI::dbDisconnect(con_db$db_con)
})


testthat::test_that("return correct class-low-connection", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_s4_class(con_db$db_con, "SQLiteConnection")

  DBI::dbDisconnect(con_db$db_con)
})

testthat::test_that("return correct class-low-data", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_s3_class(con_db$data, "tbl_df")

  DBI::dbDisconnect(con_db$db_con)
})


# Test tables -----
testthat::test_that("return corrent tables", {
  con_db <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
    )

  testthat::expect_equal(
    DBI::dbListTables(con_db$db_con),
    c(
      "AbioticData",
      "AbioticDataReference",
      "AbioticVariable",
      "AbioticVariableReference",
      "Authors",
      "BuildProducers",
      "DatasetReferences",
      "DatasetSample",
      "DatasetSourceTypeID",
      "DatasetSourceTypeReference",
      "DatasetSourcesID",
      "DatasetSourcesReference",
      "DatasetTypeID",
      "Datasets",
      "PositionLink",
      "Positions",
      "References",
      "SamplePosition",
      "SampleReference",
      "SampleSizeID",
      "SampleTaxa",
      "SampleUncertainty",
      "Samples",
      "SamplingMethodID",
      "SamplingMethodReference",
      "Taxa",
      "TaxonClassification",
      "TaxonReference",
      "TraitDerivation",
      "TraitUnitConversion",
      "Traits",
      "TraitsDomain",
      "TraitsReference",
      "TraitsValue",
      "sqlite_stat1",
      "sqlite_stat4",
      "version_control"
    )
  )

  DBI::dbDisconnect(con_db$db_con)
})


# Error handling -----

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = "not_a_file.sqlite",
      verbose = FALSE
    )
  )
})

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = 1,
      verbose = FALSE
    )
  )
})

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = 1
    )
  )
})

testthat::test_that("error handling", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = "true"
    )
  )
})

testthat::test_that("error handling - non-sqlite file", {
  path_non_sqlite <-
    base::tempfile(fileext = ".db")

  base::writeLines("not a sqlite file", path_non_sqlite)

  testthat::expect_error(
    open_vault(
      path = path_non_sqlite,
      verbose = FALSE
    )
  )

  base::unlink(path_non_sqlite)
})


# VegVault version -----
testthat::test_that("open_vault() stores the version of the highest id", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c("1.0.0", "2.0.0", "2.9.0", "2.10.0"),
      sel_layout = "2.0.0"
    )

  vault_opened <-
    open_vault(
      path = path_fixture,
      verbose = FALSE
    )

  withr::defer(
    DBI::dbDisconnect(conn = purrr::chuck(vault_opened, "db_con"))
  )

  res <-
    base::attributes(vault_opened) |>
    purrr::chuck("database_version")

  testthat::expect_equal(res, "2.10.0")
})

testthat::test_that("open_vault() keeps the pipe shape for both versions", {
  path_v1 <-
    build_trait_version_fixture(sel_version = "1.0.0")

  path_v2 <-
    build_trait_version_fixture(sel_version = "2.0.0")

  withr::defer(
    base::unlink(x = c(path_v1, path_v2))
  )

  vault_v1 <-
    open_vault(
      path = path_v1,
      verbose = FALSE
    )

  vault_v2 <-
    open_vault(
      path = path_v2,
      verbose = FALSE
    )

  withr::defer(
    DBI::dbDisconnect(conn = purrr::chuck(vault_v1, "db_con"))
  )

  withr::defer(
    DBI::dbDisconnect(conn = purrr::chuck(vault_v2, "db_con"))
  )

  res_v1 <-
    base::attributes(vault_v1) |>
    purrr::chuck("database_version")

  res_v2 <-
    base::attributes(vault_v2) |>
    purrr::chuck("database_version")

  testthat::expect_named(vault_v1, c("data", "db_con"))
  testthat::expect_named(vault_v2, c("data", "db_con"))
  testthat::expect_equal(res_v1, "1.0.0")
  testthat::expect_equal(res_v2, "2.0.0")
})

testthat::test_that("open_vault() opens the supported versions", {
  list_fixture <-
    list(
      "1.0.0" = build_version_table_fixture(
        vec_version = c("0.0.2", "0.0.3", "0.0.4", "1.0.0"),
        sel_layout = "1.0.0"
      ),
      "2.0.0" = build_version_table_fixture(
        vec_version = c("1.0.0", "2.0.0"),
        sel_layout = "2.0.0"
      ),
      "2.10.0" = build_version_table_fixture(
        vec_version = c("2.0.0", "2.10.0"),
        sel_layout = "2.0.0"
      )
    )

  purrr::iwalk(
    .x = list_fixture,
    .f = ~ {
      vault_opened <-
        open_vault(
          path = .x,
          verbose = FALSE
        )

      withr::defer(
        DBI::dbDisconnect(conn = purrr::chuck(vault_opened, "db_con"))
      )

      res <-
        base::attributes(vault_opened) |>
        purrr::chuck("database_version")

      testthat::expect_s3_class(vault_opened, "vault_pipe")
      testthat::expect_equal(res, .y)
    }
  )
})

testthat::test_that("open_vault() reads a file without versions as 1.0.0", {
  list_path <-
    list(
      no_table = build_version_table_fixture(vec_version = NULL),
      empty = build_version_table_fixture(vec_version = character(0))
    )

  purrr::iwalk(
    .x = list_path,
    .f = ~ {
      vault_opened <-
        open_vault(
          path = .x,
          verbose = FALSE
        )

      withr::defer(
        DBI::dbDisconnect(conn = purrr::chuck(vault_opened, "db_con"))
      )

      res <-
        base::attributes(vault_opened) |>
        purrr::chuck("database_version")

      testthat::expect_equal(res, "1.0.0", label = .y)
    }
  )
})

testthat::test_that("open_vault() stops for an unsupported version", {
  list_fixture <-
    list(
      older = build_version_table_fixture(
        vec_version = c("0.0.2", "0.0.4"),
        sel_layout = "1.0.0"
      ),
      newer = build_version_table_fixture(
        vec_version = c("2.0.0", "3.0.0"),
        sel_layout = "2.0.0"
      ),
      unreadable = build_version_table_fixture(
        vec_version = c("1.0.0", "2.0.0-dev"),
        sel_layout = "2.0.0"
      )
    )

  purrr::iwalk(
    .x = list_fixture,
    .f = ~ testthat::expect_error(
      open_vault(
        path = .x,
        verbose = FALSE
      ),
      label = .y
    )
  )

  testthat::expect_message(
    base::try(
      open_vault(
        path = purrr::chuck(list_fixture, "newer"),
        verbose = TRUE
      ),
      silent = TRUE
    ),
    regexp = "Update vaultkeepr"
  )
})

testthat::test_that("open_vault() closes the connection when it stops", {
  path_fixture <-
    build_version_table_fixture(
      vec_version = c("2.0.0", "3.0.0"),
      sel_layout = "2.0.0"
    )

  fn_disconnect <- DBI::dbDisconnect
  n_disconnect <- 0L

  testthat::local_mocked_bindings(
    dbDisconnect = function(conn, ...) {
      n_disconnect <<- n_disconnect + 1L
      fn_disconnect(conn, ...)
    },
    .package = "DBI"
  )

  testthat::expect_error(
    open_vault(
      path = path_fixture,
      verbose = FALSE
    )
  )

  testthat::expect_equal(n_disconnect, 1L)
})


# Read-only connection -----
testthat::test_that("open_vault() refuses to write to the database", {
  path_copy <-
    withr::local_tempfile(fileext = ".sqlite")

  base::file.copy(
    from = base::file.path(base::tempdir(), "example.sqlite"),
    to = path_copy
  )

  vault_opened <-
    open_vault(
      path = path_copy,
      verbose = FALSE
    )

  con_vault <-
    purrr::chuck(vault_opened, "db_con")

  withr::defer(
    DBI::dbDisconnect(conn = con_vault)
  )

  testthat::expect_error(
    DBI::dbExecute(
      conn = con_vault,
      statement = "CREATE TABLE written_by_test (value INTEGER)"
    ),
    regexp = "readonly"
  )
  testthat::expect_false(
    "written_by_test" %in% DBI::dbListTables(conn = con_vault)
  )
})

testthat::test_that("open_vault() leaves the file unchanged by queries", {
  path_copy <-
    withr::local_tempfile(fileext = ".sqlite")

  base::file.copy(
    from = base::file.path(base::tempdir(), "example.sqlite"),
    to = path_copy
  )

  vec_md5_before <-
    tools::md5sum(files = path_copy)

  vault_opened <-
    open_vault(
      path = path_copy,
      verbose = FALSE
    )

  data_taxa <-
    vault_opened |>
    get_datasets() |>
    get_samples() |>
    get_taxa(verbose = FALSE) |>
    purrr::chuck("data") |>
    dplyr::collect()

  DBI::dbDisconnect(conn = purrr::chuck(vault_opened, "db_con"))

  vec_md5_after <-
    tools::md5sum(files = path_copy)

  testthat::expect_gt(base::nrow(data_taxa), 0L)
  testthat::expect_equal(
    base::unname(vec_md5_after),
    base::unname(vec_md5_before)
  )
})

testthat::test_that("open_vault() still allows temporary tables", {
  path_copy <-
    withr::local_tempfile(fileext = ".sqlite")

  base::file.copy(
    from = base::file.path(base::tempdir(), "example.sqlite"),
    to = path_copy
  )

  vault_opened <-
    open_vault(
      path = path_copy,
      verbose = FALSE
    )

  con_vault <-
    purrr::chuck(vault_opened, "db_con")

  withr::defer(
    DBI::dbDisconnect(conn = con_vault)
  )

  data_computed <-
    vault_opened |>
    get_datasets() |>
    purrr::chuck("data") |>
    dplyr::compute(temporary = TRUE)

  testthat::expect_s3_class(data_computed, "tbl_sql")
  testthat::expect_gt(
    dplyr::pull(dplyr::count(data_computed), "n"),
    0L
  )
})
