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
    get_traits()

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
    get_traits()

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("number of trait ids", {
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
    get_traits()

  test_n_trait_id <-
    test_datasets$data %>%
    dplyr::distinct(trait_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_n_trait_id <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TraitsValue"),
      by = c("sample_id")
    ) %>%
    dplyr::distinct(trait_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  testthat::expect_equal(test_n_trait_id, actual_n_trait_id)

  DBI::dbDisconnect(con_db)
})

testthat::test_that("size of a total dataset", {
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
    get_traits()

  test_n_datasets <-
    test_datasets$data %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_n_datasets <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TraitsValue"),
      by = "sample_id"
    ) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  testthat::expect_equal(test_n_datasets, actual_n_datasets)

  DBI::dbDisconnect(con_db)
})

testthat::test_that("filtering the trait data by taxa data", {
  test_datasets_with_taxa <-
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
    get_traits(verbose = FALSE) %>%
    purrr::chuck("data") %>%
    dplyr::collect()

  testthat::expect_true(nrow(test_datasets_with_taxa) > 0)
  testthat::expect_true("taxon_name" %in% colnames(test_datasets_with_taxa))
  testthat::expect_equal(
    test_datasets_with_taxa %>%
      dplyr::distinct(taxon_name) %>%
      dplyr::pull("taxon_name"),
    "taxon_1"
  )
})

testthat::test_that("filtering the trait data by taxa-classified ", {
  test_datasets_with_taxa <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa(
      classify_to = "family"
    ) %>%
    get_traits(
      classify_to = "family",
      verbose = FALSE
    ) %>%
    purrr::chuck("data") %>%
    dplyr::collect()

  test_vec_taxon_id <-
    test_datasets_with_taxa %>%
    dplyr::distinct(taxon_id) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_vec_taxon_id <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "SampleTaxa"),
      by = "sample_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TaxonClassification"),
      by = "taxon_id"
    ) %>%
    dplyr::select(-"taxon_id") %>%
    dplyr::rename(
      taxon_id = "taxon_family"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TraitsValue") %>%
        dplyr::left_join(
          dplyr::tbl(con_db, "TaxonClassification"),
          by = "taxon_id"
        ) %>%
        dplyr::select(-"taxon_id") %>%
        dplyr::rename(
          taxon_id = "taxon_family"
        ),
      by = c("sample_id", "taxon_id")
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  testthat::expect_true(nrow(test_datasets_with_taxa) > 0)
  testthat::expect_true("taxon_id" %in% colnames(test_datasets_with_taxa))
  testthat::expect_equal(
    test_vec_taxon_id,
    actual_vec_taxon_id
  )

  DBI::dbDisconnect(con_db)
})

testthat::test_that("get a message when adding taxa ", {
  test_f <- function() {
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
      get_traits(verbose = TRUE)
  }

  quiet_f <-
    purrr::quietly(test_f)

  res <-
    quiet_f()

  testthat::expect_true(length(res$messages) > 0)
})

# errors -----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    get_traits()
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
      get_traits()
  )
})

testthat::test_that("error wihtout `get_samples()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_traits()
  )
})

testthat::test_that("error with bad `classify_to` class", {
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
      get_traits(
        classify_to = 123
      )
  )
})

testthat::test_that("error with bad `classify_to` value", {
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
      get_traits(
        classify_to = "pikachu"
      )
  )
})

testthat::test_that("error with bad `verbose` class", {
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
      get_traits(
        verbose = 123
      )
  )
})


# classification_data argument ----
testthat::test_that("classification_data tibble gives same genus result", {
  test_con_orig <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa(classify_to = "original")

  data_class <-
    get_classification_table(
      con = test_con_orig,
      return_raw_data = TRUE
    )

  res_with_data <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_traits(
      classify_to = "genus",
      verbose = FALSE,
      classification_data = data_class
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  res_without_data <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_traits(
      classify_to = "genus",
      verbose = FALSE
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(res_with_data, res_without_data)
})

testthat::test_that("custom classification_data overrides genus mapping", {
  test_con_orig <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa(classify_to = "original")

  data_class_custom <-
    get_classification_table(
      con = test_con_orig,
      return_raw_data = TRUE
    ) %>%
    dplyr::mutate(taxon_genus = 46L)

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
    get_traits(
      classify_to = "genus",
      verbose = FALSE,
      classification_data = data_class_custom
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(res, 46L)
})

testthat::test_that("error when classification_data is not data.frame", {
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
      get_traits(
        classify_to = "genus",
        verbose = FALSE,
        classification_data = "bad"
      )
  )
})
