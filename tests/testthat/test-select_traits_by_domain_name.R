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
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name("Plant heigh")

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
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name("Plant heigh")

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("get only correct trait domain", {
  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name("Plant heigh") %>%
    purrr::chuck("data") %>%
    dplyr::distinct(trait_domain_name) %>%
    dplyr::collect() %>%
    purrr::chuck("trait_domain_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 1)
  testthat::expect_equal(test_datasets_values, "Plant heigh")
})

testthat::test_that("get correct taxa for multi-domain selection", {
  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name(
      sel_domain = c("Stem specific density", "Diaspore mass")
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(trait_domain_name) %>%
    dplyr::collect() %>%
    purrr::chuck("trait_domain_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 2)
  testthat::expect_equal(
    test_datasets_values,
    c("Stem specific density", "Diaspore mass")
  )
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
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits()

  test_datasets_sub <-
    test_datasets_full %>%
    select_traits_by_domain_name("Plant heigh")

  nrows_full <-
    test_datasets_full$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  nrows_sub <-
    test_datasets_sub$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_true(is.numeric(nrows_full))
  testthat::expect_true(is.numeric(nrows_sub))
  testthat::expect_true(nrows_full > 0)
  testthat::expect_true(nrows_sub > 0)
  testthat::expect_true(nrows_sub < nrows_full)
})

testthat::test_that("return empty dataset", {
  nrows_empty <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    # select a domain that does not exist
    select_traits_by_domain_name("pikachu") %>%
    purrr::chuck("data") %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_equal(nrows_empty, 0)
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_traits_by_domain_name()
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
      select_traits_by_domain_name()
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
      select_traits_by_domain_name()
  )
})

testthat::test_that("error without `get_traits()`", {
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
      select_traits_by_domain_name()
  )
})

testthat::test_that("error with bad `sel_domain` class", {
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
      get_traits() %>%
      select_traits_by_domain_name(
        sel_domain = 123
      )
  )
})
