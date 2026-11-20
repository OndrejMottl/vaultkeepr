testthat::test_that("build_reference_query() returns a lazy reference table", {
  vault_datasets <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    build_reference_query(
      data_source = vault_datasets$data,
      sel_con = vault_datasets$db_con,
      sel_column = "data_source_type_id",
      sel_table = "DatasetSourceTypeReference"
    )

  testthat::expect_s3_class(res, "tbl_sql")
  testthat::expect_equal(
    colnames(res),
    c("reference_id", "reference_detail", "mandatory")
  )
})

testthat::test_that("build_reference_query() sends ids as a subquery", {
  vault_datasets <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    build_reference_query(
      data_source = vault_datasets$data,
      sel_con = vault_datasets$db_con,
      sel_column = "dataset_id",
      sel_table = "DatasetReferences"
    )

  sql_res <-
    dbplyr::sql_render(res) |>
    as.character()

  testthat::expect_match(sql_res, "IN \\(SELECT")
  testthat::expect_no_match(sql_res, "IN \\(\\s*[0-9]")
})

testthat::test_that("build_reference_query() returns the linked references", {
  db_con <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      ":memory:"
    )

  withr::defer(DBI::dbDisconnect(db_con))

  DBI::dbWriteTable(
    db_con,
    "Selection",
    tibble::tibble(dataset_id = c(1L, 1L, 2L, NA_integer_))
  )

  DBI::dbWriteTable(
    db_con,
    "DatasetReferences",
    tibble::tibble(
      dataset_id = c(1L, 1L, 2L, 3L),
      reference_id = c(10L, 11L, 99L, 12L)
    )
  )

  DBI::dbWriteTable(
    db_con,
    "References",
    tibble::tibble(
      reference_id = c(10L, 11L, 12L),
      reference_detail = c("ref_10", "ref_11", "ref_12"),
      mandatory = c(1L, 0L, 1L)
    )
  )

  res <-
    build_reference_query(
      data_source = dplyr::tbl(db_con, "Selection"),
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
