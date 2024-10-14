# datasets ----
testthat::test_that("return dataset", {
  test_valut <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets()

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = FALSE
    )

  testthat::expect_s3_class(data_packed, "data.frame")

  testthat::expect_true(
    all(
      c(
        "dataset_name", "data_source_desc", "dataset_type",
        "dataset_source_type", "coord_long", "coord_lat",
        "sampling_method_details"
      ) %in% colnames(data_packed)
    )
  )
})

# samples data ----
testthat::test_that("return samples", {
  test_valut <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples()

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = FALSE
    )

  testthat::expect_true(
    "data_samples" %in% colnames(data_packed)
  )

  vec_samples_colnames <-
    data_packed %>%
    dplyr::select("data_samples") %>%
    tidyr::unnest("data_samples") %>%
    colnames()

  testthat::expect_true(
    all(
      vec_samples_colnames %in% c(
        "sample_name", "age", "sample_size", "sample_details", "description"
      )
    )
  )
})

# community data ----
testthat::test_that("return community", {
  test_valut <-
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

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = FALSE
    )

  testthat::expect_true(
    "data_community" %in% colnames(data_packed)
  )

  vec_community_colnames <-
    data_packed %>%
    dplyr::select("data_community") %>%
    tidyr::unnest("data_community") %>%
    colnames()

  testthat::expect_true(
    all(
      stringr::str_detect(vec_community_colnames, "taxon_") |
        "sample_name" %in% vec_community_colnames
    )
  )
})

# trait data -----
testthat::test_that("return trait data", {
  test_valut <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_traits(verbose = FALSE)

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = FALSE
    )

  testthat::expect_true(
    "data_traits" %in% colnames(data_packed)
  )

  vec_traits_colnames <-
    data_packed %>%
    dplyr::select("data_traits") %>%
    tidyr::unnest("data_traits") %>%
    colnames()

  testthat::expect_true(
    all(
      stringr::str_detect(vec_traits_colnames, "taxon_") |
        vec_traits_colnames %in% c(
          "sample_name", "trait_domain_name", "trait_name"
        )
    )
  )
})

testthat::test_that("return trait data - with taxa", {
  test_valut <-
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

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = FALSE
    )

  testthat::expect_true(
    "data_traits" %in% colnames(data_packed)
  )

  vec_traits_colnames <-
    data_packed %>%
    dplyr::select("data_traits") %>%
    tidyr::unnest("data_traits") %>%
    colnames()

  testthat::expect_true(
    all(
      stringr::str_detect(vec_traits_colnames, "taxon_") |
        vec_traits_colnames %in% c(
          "sample_name", "trait_domain_name", "trait_name"
        )
    )
  )
})

# abiotic data -----
testthat::test_that("return abiotic data", {
  test_valut <-
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

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = FALSE
    )

  testthat::expect_true(
    "data_abiotic" %in% colnames(data_packed)
  )

  vec_abiotic_colnames <-
    data_packed %>%
    dplyr::select("data_abiotic") %>%
    tidyr::unnest("data_abiotic") %>%
    colnames()

  testthat::expect_true(
    all(
      vec_abiotic_colnames %in% c(
        "sample_name", "dataset_name_abiotic", "sample_name_abiotic",
        "abiotic_variable_name", "abiotic_value",
        "abiotic_variable_unit",
        "measure_details"
      )
    )
  )
})

# errors -----
testthat::test_that("error with bad `sel_data`", {
  testthat::expect_error(
    pack_data(sel_data = 123)
  )
})

testthat::test_that("foget to collect the data", {
  test_valut <-
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

  testthat::expect_error(
    pack_data(
      sel_data = test_valut,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `verbose`", {
  test_valut <-
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

  data_readble <-
    get_readable_column_names(
      con = test_valut$db_con,
      data = test_valut$data
    ) %>%
    dplyr::collect()


  testthat::expect_error(
    pack_data(
      sel_data = data_readble,
      verbose = "bad"
    )
  )
})
