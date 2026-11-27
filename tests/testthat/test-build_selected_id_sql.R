testthat::test_that("build_selected_id_sql() returns SQL of distinct ids", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  res <-
    build_selected_id_sql(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_column = "sample_id"
    )

  testthat::expect_s3_class(res, "sql")
  testthat::expect_match(
    base::as.character(res),
    "^SELECT DISTINCT `sample_id`"
  )
})

testthat::test_that("build_selected_id_sql() keeps the ids of the selection", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_selected_id <-
    build_selected_id_sql(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_column = "sample_id"
    )

  vec_sample_id <-
    dplyr::tbl(
      src = purrr::chuck(vault_selected, "db_con"),
      from = "SampleUncertainty"
    ) |>
    dplyr::filter(
      .data[["sample_id"]] %in% !!sql_selected_id
    ) |>
    dplyr::pull(var = "sample_id")

  testthat::expect_equal(
    base::sort(base::unique(vec_sample_id)),
    c(1L, 2L, 3L)
  )
})

testthat::test_that("build_selected_id_sql() rejects a collected table", {
  data_collected <-
    tibble::tibble(sample_id = 1L)

  testthat::expect_error(
    base::suppressMessages(
      build_selected_id_sql(
        data_source = data_collected,
        sel_column = "sample_id"
      )
    )
  )

  testthat::expect_message(
    base::try(
      build_selected_id_sql(
        data_source = data_collected,
        sel_column = "sample_id"
      ),
      silent = TRUE
    ),
    regexp = "tbl_sql"
  )
})

testthat::test_that("build_selected_id_sql() rejects a missing column", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  testthat::expect_error(
    base::suppressMessages(
      build_selected_id_sql(
        data_source = purrr::chuck(vault_selected, "data"),
        sel_column = "dataset_id"
      )
    )
  )

  testthat::expect_message(
    base::try(
      build_selected_id_sql(
        data_source = purrr::chuck(vault_selected, "data"),
        sel_column = "dataset_id"
      ),
      silent = TRUE
    ),
    regexp = "dataset_id"
  )
})
