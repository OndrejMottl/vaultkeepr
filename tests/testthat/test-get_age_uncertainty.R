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
