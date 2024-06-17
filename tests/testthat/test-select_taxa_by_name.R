testthat::test_that("return correct class-high", {
  test_datasets <-
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
    select_taxa_by_name("taxon_1")

  testthat::expect_s3_class(test_datasets, "vault_pipe")
})

testthat::test_that("basic data.frame structure", {
  test_datasets <-
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
    select_taxa_by_name("taxon_1")

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("get only correct taxa", {
  test_datasets_values <-
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
    select_taxa_by_name("taxon_1") %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_name) %>%
    dplyr::collect() %>%
    purrr::chuck("taxon_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 1)
  testthat::expect_equal(test_datasets_values, "taxon_1")
})

testthat::test_that("get correct taxa for multi-taxa selection", {
  test_datasets_values <-
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
      sel_taxa = c("taxon_2", "taxon_3")
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_name) %>%
    dplyr::collect() %>%
    purrr::chuck("taxon_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 2)
  testthat::expect_equal(test_datasets_values, c("taxon_2", "taxon_3"))
})

testthat::test_that("subset dataset", {
  test_datasets_full <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa()

  test_datasets_sub <-
    test_datasets_full %>%
    select_taxa_by_name("taxon_1")

  nrows_full <-
    test_datasets_full$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  nrows_sub <-
    test_datasets_sub$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_true(nrows_sub < nrows_full)
})

testthat::test_that("return empy dataset", {
  nrows_empty <-
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
    # select a taxon that does not exist
    select_taxa_by_name("pikachu") %>%
    purrr::chuck("data") %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_equal(nrows_empty, 0)
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_taxa_by_name()
  )
})

testthat::test_that("error wihtout `get_datasets()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      select_taxa_by_name()
  )
})

testthat::test_that("error without `get_samples()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_taxa_by_name()
  )
})

testthat::test_that("error without `get_taxa()`", {
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
      select_taxa_by_name()
  )
})

testthat::test_that("error with bad `sel_taxa` class", {
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
      get_taxa() %>%
      select_taxa_by_name(
        sel_taxa = 123
      )
  )
})
