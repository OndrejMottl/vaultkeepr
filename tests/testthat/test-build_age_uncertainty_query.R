testthat::test_that("build_age_uncertainty_query() returns a lazy table", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  res <-
    build_age_uncertainty_query(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_con = purrr::chuck(vault_selected, "db_con")
    )

  testthat::expect_s3_class(res, "tbl_sql")
  testthat::expect_equal(
    base::colnames(res),
    c("sample_id", "iteration", "age_uncertainty")
  )
})

testthat::test_that("build_age_uncertainty_query() keeps selected samples", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  data_res <-
    build_age_uncertainty_query(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_con = purrr::chuck(vault_selected, "db_con")
    ) |>
    dplyr::collect() |>
    dplyr::arrange(
      .data[["sample_id"]],
      .data[["iteration"]]
    )

  data_expected <-
    tibble::tibble(
      sample_id = c(1L, 1L, 2L, 2L, 3L, 3L),
      iteration = c(1L, 2L, 1L, 2L, 1L, 2L),
      age_uncertainty = c(110L, 120L, 210L, 220L, 310L, 320L)
    )

  testthat::expect_equal(data_res, data_expected)
})

# Query shape -----

testthat::test_that("build_age_uncertainty_query() sends ids as a subquery", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_res <-
    build_age_uncertainty_query(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_con = purrr::chuck(vault_selected, "db_con")
    ) |>
    dbplyr::sql_render() |>
    base::as.character()

  # A correlated EXISTS runs the selection once for every row of the table
  testthat::expect_no_match(sql_res, "EXISTS")
  testthat::expect_match(
    sql_res,
    "`sample_id` IN \\(SELECT DISTINCT `sample_id`"
  )
})

testthat::test_that("build_age_uncertainty_query() uses the sample_id index", {
  vault_selected <-
    build_age_uncertainty_fixture(flag_primary_key = FALSE)

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_res <-
    build_age_uncertainty_query(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_con = purrr::chuck(vault_selected, "db_con")
    ) |>
    dbplyr::sql_render()

  vec_plan_detail <-
    DBI::dbGetQuery(
      conn = purrr::chuck(vault_selected, "db_con"),
      statement = stringr::str_c("EXPLAIN QUERY PLAN ", sql_res)
    ) |>
    dplyr::pull(var = "detail")

  testthat::expect_true(
    base::any(
      stringr::str_detect(
        string = vec_plan_detail,
        pattern = stringr::str_c(
          "^SEARCH SampleUncertainty ",
          "USING INDEX idx_sampleuncertainty_sample_id"
        )
      )
    )
  )
})

testthat::test_that("build_age_uncertainty_query() uses the 2.0.0 key", {
  vault_selected <-
    build_age_uncertainty_fixture(flag_primary_key = TRUE)

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_res <-
    build_age_uncertainty_query(
      data_source = purrr::chuck(vault_selected, "data"),
      sel_con = purrr::chuck(vault_selected, "db_con")
    ) |>
    dbplyr::sql_render()

  vec_plan_detail <-
    DBI::dbGetQuery(
      conn = purrr::chuck(vault_selected, "db_con"),
      statement = stringr::str_c("EXPLAIN QUERY PLAN ", sql_res)
    ) |>
    dplyr::pull(var = "detail")

  testthat::expect_true(
    base::any(
      stringr::str_detect(
        string = vec_plan_detail,
        pattern = stringr::str_c(
          "^SEARCH SampleUncertainty ",
          "USING PRIMARY KEY \\(sample_id=\\?\\)"
        )
      )
    )
  )
})
