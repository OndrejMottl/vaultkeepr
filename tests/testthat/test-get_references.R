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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
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
    extract_data() %>%
    get_references(
      data_source = .,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "AbioticVariable",
      verbose = FALSE
    )

  test_refs %>%
    purrr::chuck("reference_detail") %>%
    stringr::str_detect("abiotic_variable") %>%
    all() %>%
    testthat::expect_true()
})


testthat::test_that("return null - data_source - Dataset - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Dataset",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - DatasetSource - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "DatasetSource",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - DatasetSourceType - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "DatasetSourceType",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - SamplingMethod - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "SamplingMethod",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - Sample - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Sample",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - Taxon - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Taxon",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - Trait - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "Trait",
      verbose = FALSE
    )
  )
})

testthat::test_that("return null - data_source - AbioticVariable - empty", {
  testthat::expect_null(
    get_references(
      data_source = tibble::tibble(),
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      type = "AbioticVariable",
      verbose = FALSE
    )
  )
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
testthat::test_that("error - data_source - empty", {
  testthat::expect_error(
    get_references(
      data_source = NULL,
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      ),
      verbose = FALSE
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
      ),
      verbose = FALSE
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
      extract_data() %>%
      get_references(
        data_source = .,
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        ),
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
      extract_data() %>%
      get_references(
        data_source = .,
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        ),
        type = "Dataset",
        verbose = "wrong_type"
      )
  )
})
