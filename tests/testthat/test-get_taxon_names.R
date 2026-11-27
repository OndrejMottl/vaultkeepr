testthat::test_that("output is a tibble", {
  res <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa() %>%
    get_taxon_names()

  testthat::expect_s3_class(res, "tbl_df")
})

testthat::test_that("output has exactly one column named taxon_name", {
  res <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa() %>%
    get_taxon_names()

  testthat::expect_equal(base::ncol(res), 1L)
  testthat::expect_equal(base::colnames(res), "taxon_name")
})

testthat::test_that("output has 45 rows for full unfiltered plan", {
  res <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa() %>%
    get_taxon_names()

  testthat::expect_equal(base::nrow(res), 45L)
})

testthat::test_that("taxon_name column is sorted alphabetically", {
  res <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa() %>%
    get_taxon_names()

  vec_names <-
    dplyr::pull(res, "taxon_name")

  testthat::expect_equal(vec_names, base::sort(vec_names))
})

testthat::test_that("filtering by taxa name reduces the result", {
  res <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa() %>%
    select_taxa_by_name(
      sel_taxa = c("taxon_1", "taxon_2", "taxon_3")
    ) %>%
    get_taxon_names()

  testthat::expect_lt(base::nrow(res), 45L)
  testthat::expect_gte(base::nrow(res), 1L)
})

testthat::test_that("genus classification returns 9 rows", {
  res <-
    suppressWarnings(
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_taxa(classify_to = "genus")
    ) %>%
    get_taxon_names()

  testthat::expect_equal(base::nrow(res), 9L)
})

testthat::test_that("genus result is still a tibble with taxon_name column", {
  res <-
    suppressWarnings(
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_taxa(classify_to = "genus")
    ) %>%
    get_taxon_names()

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(base::colnames(res), "taxon_name")
})

testthat::test_that("error when called with no argument", {
  testthat::expect_error(
    get_taxon_names()
  )
})

testthat::test_that("error when con is not a vault_pipe", {
  testthat::expect_error(
    get_taxon_names(con = base::list(data = NULL, db_con = NULL))
  )
})

testthat::test_that("error when get_taxa() was not called upstream", {
  plan <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples()

  testthat::expect_error(
    get_taxon_names(con = plan)
  )
})

# Classified taxa of a selection -----

testthat::test_that("get_taxon_names() lists classified taxa as before", {
  vault_selected <-
    build_classification_fixture()

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sel_con <-
    purrr::chuck(vault_selected, "db_con")

  data_selected <-
    purrr::chuck(vault_selected, "data") |>
    dplyr::collect()

  data_sample_taxa <-
    dplyr::tbl(
      src = sel_con,
      from = "SampleTaxa"
    ) |>
    dplyr::collect()

  data_classification <-
    dplyr::tbl(
      src = sel_con,
      from = "TaxonClassification"
    ) |>
    dplyr::collect()

  data_taxa <-
    dplyr::tbl(
      src = sel_con,
      from = "Taxa"
    ) |>
    dplyr::collect()

  data_expected_taxa <-
    build_expected_classified_values(
      data_selected = data_selected,
      data_values = data_sample_taxa,
      data_classification = data_classification,
      sel_rank_column = "taxon_genus",
      vec_join_column = "sample_id",
      sel_suffix = "_vegetation"
    )

  data_expected <-
    data_expected_taxa |>
    dplyr::filter(
      !base::is.na(.data[["taxon_id"]])
    ) |>
    dplyr::distinct(
      dplyr::across(
        dplyr::all_of("taxon_id")
      )
    ) |>
    dplyr::left_join(
      y = data_taxa,
      by = dplyr::join_by("taxon_id"),
      na_matches = "never"
    ) |>
    dplyr::distinct(
      dplyr::across(
        dplyr::all_of("taxon_name")
      )
    ) |>
    dplyr::filter(
      !base::is.na(.data[["taxon_name"]])
    ) |>
    dplyr::arrange(.data[["taxon_name"]])

  res <-
    base::suppressWarnings(
      get_taxa(
        con = vault_selected,
        classify_to = "genus",
        verbose = FALSE
      )
    ) |>
    get_taxon_names()

  testthat::expect_equal(res, data_expected)
})
