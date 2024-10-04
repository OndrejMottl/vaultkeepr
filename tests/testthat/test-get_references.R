# single reference types
testthat::test_that("Dataset", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Dataset"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("dataset") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("DatasetSource", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "DatasetSource"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("data_source") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("DatasetSourceType", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "DatasetSourceType"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("data_source_type") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("SamplingMethod", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "SamplingMethod"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("sampling_method") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("Sample", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Sample"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("sample") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("Taxon", {
  test_refs <-
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Taxon"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("taxon") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("Trait", {
  test_refs <-
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Trait"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("trait") %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("AbioticVariable", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(verbose = FALSE) %>%
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "AbioticVariable"
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("abiotic_variable") %>%
    all() %>%
    testthat::expect_true()
})

# multiple reference types
