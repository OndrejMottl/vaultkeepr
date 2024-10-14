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
    get_references(
      type = "Dataset",
      verbose = FALSE
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
    get_references(
      type = "DatasetSource",
      verbose = FALSE
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
    get_references(
      type = "DatasetSourceType",
      verbose = FALSE
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
    get_references(
      type = "SamplingMethod",
      verbose = FALSE
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
    get_references(
      type = "Sample",
      verbose = FALSE
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
    get_references(
      type = "Taxon",
      verbose = FALSE
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
    get_references(
      type = "Trait",
      verbose = FALSE
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
    get_references(
      type = "AbioticVariable",
      verbose = FALSE
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("abiotic_variable") %>%
    all() %>%
    testthat::expect_true()
})

# multiple reference types
testthat::test_that("Dataset-alike", {
  test_refs <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_references(
      type = c("Dataset", "DatasetSource", "DatasetSourceType"),
      verbose = FALSE
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    {
      stringr::str_detect(., "dataset") |
        stringr::str_detect(., "data_source") |
        stringr::str_detect(., "data_source_type")
    } %>%
    all() %>%
    testthat::expect_true()
})

testthat::test_that("all types", {
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
    get_traits(verbose = FALSE) %>%
    get_abiotic_data(verbose = FALSE) %>%
    get_references(
      type = c(
        "Dataset", "DatasetSource", "DatasetSourceType",
        "SamplingMethod",
        "Sample",
        "Taxon",
        "Trait",
        "AbioticVariable"
      ),
      verbose = FALSE
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    {
      stringr::str_detect(., "dataset") |
        stringr::str_detect(., "data_source") |
        stringr::str_detect(., "data_source_type") |
        stringr::str_detect(., "sampling_method") |
        stringr::str_detect(., "sample") |
        stringr::str_detect(., "taxon") |
        stringr::str_detect(., "trait") |
        stringr::str_detect(., "abiotic_variable")
    } %>%
    all() %>%
    testthat::expect_true()
})

# error handling ----
testthat::test_that("error - con - empty", {
  testthat::expect_error(
    get_references()
  )
})

testthat::test_that("error - con - wrong class", {
  testthat::expect_error(
    get_references(con = 1:10)
  )
})

testthat::test_that("error - Dataset - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_references(
        type = "Dataset",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - DatasetSource - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_references(
        type = "DatasetSource",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - DatasetSourceType - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_references(
        type = "DatasetSourceType",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - SamplingMethod - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_references(
        type = "SamplingMethod",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - Sample - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = "Sample",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - Taxon - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_references(
        type = "Taxon",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - Trait - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = "Trait",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - AbioticVariable - empty", {
  testthat::expect_null(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = "AbioticVariable",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - type - empty", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = NULL,
        verbose = FALSE
      )
  )
})

testthat::test_that("error - type - wrong class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = 1:10,
        verbose = FALSE
      )
  )
})

testthat::test_that("error - type - wrong type", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = "wrong_type",
        verbose = FALSE
      )
  )
})

testthat::test_that("error - verbose - wrong class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = "Dataset",
        verbose = 1:10
      )
  )
})

testthat::test_that("error - verbose - wrong type", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_references(
        type = "Dataset",
        verbose = "wrong_type"
      )
  )
})
