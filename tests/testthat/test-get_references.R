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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = c("Dataset", "DatasetSource", "DatasetSourceType")
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = c(
        "Dataset", "DatasetSource", "DatasetSourceType",
        "SamplingMethod",
        "Sample",
        "Taxon",
        "Trait",
        "AbioticVariable"
      )
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
testthat::test_that("error - data_source - empty", {
  testthat::expect_error(
    get_references(
      data_source = NULL,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )
  )
})

testthat::test_that("error - data_source - wrong class", {
  testthat::expect_error(
    get_references(
      data_source = 1:10,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )
  )
})

testthat::test_that("error - data_source - Dataset - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Dataset"
    )
  )
})

testthat::test_that("error - data_source - DatasetSource - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "DatasetSource"
    )
  )
})

testthat::test_that("error - data_source - DatasetSourceType - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "DatasetSourceType"
    )
  )
})

testthat::test_that("error - data_source - SamplingMethod - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "SamplingMethod"
    )
  )
})

testthat::test_that("error - data_source - Sample - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Sample"
    )
  )
})

testthat::test_that("error - data_source - Taxon - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Taxon"
    )
  )
})

testthat::test_that("error - data_source - Trait - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Trait"
    )
  )
})

testthat::test_that("error - data_source - AbioticVariable - empty", {
  testthat::expect_error(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "AbioticVariable"
    )
  )
})

testthat::test_that("error - path - empty", {
  testthat::expect_error(
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
        path = NULL,
        type = "Dataset"
      )
  )
})

testthat::test_that("error - path - wrong class", {
  testthat::expect_error(
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
        path = 1:10,
        type = "Dataset"
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
      extract_data() %>%
      get_references(
        data_source = .,
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        ),
        type = NULL
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
      extract_data() %>%
      get_references(
        data_source = .,
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        ),
        type = 1:10
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
      extract_data() %>%
      get_references(
        data_source = .,
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        ),
        type = "wrong_type"
      )
  )
})
