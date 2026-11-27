testthat::test_that("build_reference_query() returns a lazy reference table", {
  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    build_reference_query(
      data_source = purrr::chuck(vault_datasets, "data"),
      sel_con = purrr::chuck(vault_datasets, "db_con"),
      sel_column = "data_source_type_id",
      sel_table = "DatasetSourceTypeReference"
    )

  testthat::expect_s3_class(res, "tbl_sql")
  testthat::expect_equal(
    base::colnames(res),
    c("reference_id", "reference_detail", "mandatory")
  )
})

testthat::test_that("build_reference_query() sends distinct ids as subquery", {
  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    build_reference_query(
      data_source = purrr::chuck(vault_datasets, "data"),
      sel_con = purrr::chuck(vault_datasets, "db_con"),
      sel_column = "dataset_id",
      sel_table = "DatasetReferences"
    )

  sql_res <-
    dbplyr::sql_render(res) |>
    base::as.character()

  # DISTINCT lets SQLite skip the joins that do not change the ids; without
  #   it the subquery was about twice as slow as the old literal list
  testthat::expect_match(sql_res, "IN \\(SELECT DISTINCT")
  testthat::expect_no_match(sql_res, "IN \\(\\s*[0-9]")
})

testthat::test_that("build_reference_query() returns the linked references", {
  db_con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    expr = DBI::dbDisconnect(conn = db_con)
  )

  DBI::dbWriteTable(
    conn = db_con,
    name = "Selection",
    value = tibble::tibble(dataset_id = c(1L, 1L, 2L, NA_integer_))
  )

  DBI::dbWriteTable(
    conn = db_con,
    name = "DatasetReferences",
    value = tibble::tibble(
      dataset_id = c(1L, 1L, 2L, 3L),
      reference_id = c(10L, 11L, 99L, 12L)
    )
  )

  DBI::dbWriteTable(
    conn = db_con,
    name = "References",
    value = tibble::tibble(
      reference_id = c(10L, 11L, 12L),
      reference_detail = c("ref_10", "ref_11", "ref_12"),
      mandatory = c(1L, 0L, 1L)
    )
  )

  data_selection <-
    dplyr::tbl(
      src = db_con,
      from = "Selection"
    )

  res <-
    build_reference_query(
      data_source = data_selection,
      sel_con = db_con,
      sel_column = "dataset_id",
      sel_table = "DatasetReferences"
    ) |>
    dplyr::collect() |>
    dplyr::arrange(reference_id)

  testthat::expect_equal(
    dplyr::pull(res, reference_id),
    c(10L, 11L, 99L)
  )
  testthat::expect_equal(
    dplyr::pull(res, reference_detail),
    c("ref_10", "ref_11", NA_character_)
  )
  testthat::expect_equal(
    dplyr::pull(res, mandatory),
    c(1L, 0L, NA_integer_)
  )
})
