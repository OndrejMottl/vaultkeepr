testthat::test_that("returns a tibble not vault_pipe", {
  test_con <-
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

  res <-
    get_classification_table(con = test_con)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_false(inherits(res, "vault_pipe"))
})

testthat::test_that("default returns human-readable column names", {
  test_con <-
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

  res <-
    get_classification_table(con = test_con)

  testthat::expect_equal(
    base::colnames(res),
    c(
      "taxon_id",
      "taxon_name",
      "taxon_species",
      "species_name",
      "taxon_genus",
      "genus_name",
      "taxon_family",
      "family_name"
    )
  )
})

testthat::test_that("return_raw_data = TRUE returns raw ID columns", {
  test_con <-
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

  res <-
    get_classification_table(
      con = test_con,
      return_raw_data = TRUE
    )

  testthat::expect_equal(
    base::colnames(res),
    c("taxon_id", "taxon_species", "taxon_genus", "taxon_family")
  )
})

testthat::test_that("return_raw_data = TRUE restricted to taxa in data", {
  test_con <-
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

  res <-
    get_classification_table(
      con = test_con,
      return_raw_data = TRUE
    )

  vec_taxa_in_data <-
    test_con$data %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  vec_taxa_in_res <-
    dplyr::pull(res, taxon_id)

  testthat::expect_true(
    base::all(vec_taxa_in_res %in% vec_taxa_in_data)
  )
})

testthat::test_that("return_raw_data = TRUE result is usable in get_taxa", {
  test_con <-
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
      con = test_con,
      return_raw_data = TRUE
    )

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
    get_taxa(
      classify_to = "genus",
      classification_data = data_class
    )

  testthat::expect_s3_class(res, "vault_pipe")

  vec_taxa_genus <-
    res$data %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  testthat::expect_true(base::all(vec_taxa_genus >= 46))
  testthat::expect_true(base::all(vec_taxa_genus < 55))
})

testthat::test_that("default result has correct number of rows", {
  test_con <-
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

  res <-
    get_classification_table(con = test_con)

  vec_taxa_in_data <-
    test_con$data %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(
    base::nrow(res),
    base::length(vec_taxa_in_data)
  )
})

# errors ----
testthat::test_that("error when con is not vault_pipe", {
  testthat::expect_error(
    get_classification_table(con = "bad")
  )
})

testthat::test_that("error when con$data has no taxon_id column", {
  test_con <-
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
    get_classification_table(con = test_con)
  )
})

testthat::test_that("error when return_raw_data is not logical", {
  test_con <-
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

  testthat::expect_error(
    get_classification_table(
      con = test_con,
      return_raw_data = "yes"
    )
  )
})
