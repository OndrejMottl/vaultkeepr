testthat::test_that("build_mandatory_reference_query() is NULL without ids", {
  vault_empty <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    )

  res <-
    build_mandatory_reference_query(
      data_source = purrr::chuck(vault_empty, "data"),
      sel_con = purrr::chuck(vault_empty, "db_con")
    )

  testthat::expect_null(res)
})

testthat::test_that("build_mandatory_reference_query() is one lazy query", {
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
    build_mandatory_reference_query(
      data_source = purrr::chuck(vault_datasets, "data"),
      sel_con = purrr::chuck(vault_datasets, "db_con")
    )

  sql_res <-
    dbplyr::sql_render(res) |>
    base::as.character()

  testthat::expect_s3_class(res, "tbl_sql")
  testthat::expect_match(sql_res, "LIMIT 1")
  testthat::expect_match(sql_res, "IN \\(SELECT")
  testthat::expect_no_match(sql_res, "IN \\(\\s*[0-9]")
  testthat::expect_equal(
    stringr::str_count(
      string = sql_res,
      pattern = "FROM `Datasets`"
    ),
    1L
  )
})
