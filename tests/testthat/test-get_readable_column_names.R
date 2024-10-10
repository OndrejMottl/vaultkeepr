testthat::test_that("return correct class", {
  test_vault <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets()

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_s3_class(test_data, "tbl_sql")
})

testthat::test_that("colnames - dataset", {
  test_vault <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets()

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_true(
    all(
      c(
        "dataset_name", "data_source_desc",
        "dataset_type", "dataset_source_type",
        "coord_long", "coord_lat", "sampling_method_details"
      ) %in%
        colnames(test_data)
    )
  )
})

testthat::test_that("colnames - dataset + samples", {
  test_vault <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples()

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_true(
    all(
      c(
        "sample_name", "sample_details", "age", "sample_size", "description"
      ) %in%
        colnames(test_data)
    )
  )
})

testthat::test_that("colnames - dataset + samples + taxa", {
  test_vault <-
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

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_true(
    all(
      c(
        "taxon_name", "value"
      ) %in%
        colnames(test_data)
    )
  )
})

testthat::test_that("colnames - dataset + samples + trait", {
  test_vault <-
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

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_true(
    all(
      c(
        "trait_domain_name", "trait_domanin_description",
        "trait_name", "taxon_name", "trait_value"
      ) %in%
        colnames(test_data)
    )
  )
})

testthat::test_that("colnames - dataset + samples + abiotic", {
  test_vault <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_abiotic_data(verbose = FALSE)

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_true(
    all(
      c(
        "abiotic_variable_name", "abiotic_value",
        "abiotic_variable_unit", "measure_details",
        "sample_name_gridpoint"
      ) %in%
        colnames(test_data)
    )
  )
})


testthat::test_that("colnames - dataset + samples + taxa + trait", {
  test_vault <-
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
    get_traits(verbose = FALSE)

  test_data <-
    get_readable_column_names(
      con = test_vault$db_con,
      data = test_vault$data
    )

  testthat::expect_true(
    all(
      c(
        "taxon_name", "value",
        "trait_domain_name", "trait_domanin_description",
        "trait_name", "taxon_name_trait", "trait_value"
      ) %in%
        colnames(test_data)
    )
  )
})