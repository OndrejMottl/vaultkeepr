testthat::test_that("returns a tibble", {
  test_result <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_age_uncertainty()

  testthat::expect_s3_class(test_result, "tbl_df")
})

testthat::test_that("returns a data.frame", {
  test_result <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_age_uncertainty()

  testthat::expect_true(base::is.data.frame(test_result))
})

testthat::test_that(
  "default output has sample_name and iteration cols",
  {
    test_result <-
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_age_uncertainty()

    test_col_names <-
      base::colnames(test_result)

    testthat::expect_equal(
      base::sort(test_col_names),
      base::sort(
        base::c(
          "sample_name",
          "iteration_1",
          "iteration_2",
          "iteration_3",
          "iteration_4",
          "iteration_5"
        )
      )
    )
  }
)

testthat::test_that(
  "default row count equals number of distinct samples",
  {
    test_n_distinct_samples <-
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      purrr::chuck("data") %>%
      dplyr::distinct(sample_id) %>%
      dplyr::count(name = "N") %>%
      dplyr::collect() %>%
      dplyr::pull("N")

    test_n_result <-
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_age_uncertainty() %>%
      dplyr::count(name = "N") %>%
      dplyr::pull("N")

    testthat::expect_equal(
      test_n_result,
      test_n_distinct_samples
    )
  }
)

testthat::test_that(
  "return_raw_data TRUE has only uncertainty cols",
  {
    test_result <-
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_age_uncertainty(return_raw_data = TRUE)

    test_col_names <-
      base::colnames(test_result)

    testthat::expect_equal(
      base::sort(test_col_names),
      base::sort(
        base::c("sample_id", "iteration", "age_uncertainty")
      )
    )
  }
)

testthat::test_that(
  "return_raw_data TRUE row count is 5x distinct samples",
  {
    test_n_distinct_samples <-
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      purrr::chuck("data") %>%
      dplyr::distinct(sample_id) %>%
      dplyr::count(name = "N") %>%
      dplyr::collect() %>%
      dplyr::pull("N")

    test_n_result <-
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_age_uncertainty(return_raw_data = TRUE) %>%
      dplyr::count(name = "N") %>%
      dplyr::pull("N")

    testthat::expect_equal(
      test_n_result,
      test_n_distinct_samples * 5L
    )
  }
)

# Selected samples and row order -----

testthat::test_that("get_age_uncertainty() sorts raw rows by sample", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  res <-
    get_age_uncertainty(
      con = vault_selected,
      return_raw_data = TRUE
    )

  data_expected <-
    tibble::tibble(
      sample_id = c(1L, 1L, 2L, 2L, 3L, 3L),
      iteration = c(1L, 2L, 1L, 2L, 1L, 2L),
      age_uncertainty = c(110L, 120L, 210L, 220L, 310L, 320L)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("get_age_uncertainty() gives one wide row per sample", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  res <-
    get_age_uncertainty(
      con = vault_selected,
      return_raw_data = FALSE
    )

  data_expected <-
    tibble::tibble(
      sample_name = c("sample_1", "sample_2", "sample_3"),
      iteration_1 = c(110L, 210L, 310L),
      iteration_2 = c(120L, 220L, 320L)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("get_age_uncertainty() reads the 2.0.0 layout", {
  vault_selected <-
    build_age_uncertainty_fixture(flag_primary_key = TRUE)

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  res <-
    get_age_uncertainty(
      con = vault_selected,
      return_raw_data = TRUE
    )

  data_expected <-
    tibble::tibble(
      sample_id = c(1L, 1L, 2L, 2L, 3L, 3L),
      iteration = c(1L, 2L, 1L, 2L, 1L, 2L),
      age_uncertainty = c(110L, 120L, 210L, 220L, 310L, 320L)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("get_age_uncertainty() returns no rows without samples", {
  vault_selected <-
    build_age_uncertainty_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  data_samples_none <-
    purrr::chuck(vault_selected, "data") |>
    dplyr::filter(
      .data[["sample_id"]] > 10L
    )

  vault_empty <-
    base::structure(
      base::list(
        data = data_samples_none,
        db_con = purrr::chuck(vault_selected, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  res_raw <-
    get_age_uncertainty(
      con = vault_empty,
      return_raw_data = TRUE
    )

  res_wide <-
    get_age_uncertainty(
      con = vault_empty,
      return_raw_data = FALSE
    )

  testthat::expect_equal(base::nrow(res_raw), 0L)
  testthat::expect_equal(
    base::colnames(res_raw),
    c("sample_id", "iteration", "age_uncertainty")
  )
  testthat::expect_equal(base::nrow(res_wide), 0L)
  testthat::expect_equal(base::colnames(res_wide), "sample_name")
})

# errors -----
testthat::test_that("error when con is not vault_pipe", {
  testthat::expect_error(
    get_age_uncertainty(con = "not_a_vault_pipe")
  )
})

testthat::test_that("error when sample_id not in pipe data", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_age_uncertainty()
  )
})

testthat::test_that("error when return_raw_data is not logical", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_age_uncertainty(return_raw_data = "yes")
  )
})
