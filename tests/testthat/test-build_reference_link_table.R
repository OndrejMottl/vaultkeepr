testthat::test_that("build_reference_link_table() returns one row per type", {
  res <-
    build_reference_link_table()

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("reference_source", "sel_column", "sel_table")
  )
  testthat::expect_equal(
    dplyr::pull(res, reference_source),
    c(
      "Dataset",
      "DatasetSource",
      "DatasetSourceType",
      "SamplingMethod",
      "Sample",
      "Taxon",
      "Trait",
      "AbioticVariable"
    )
  )
})

testthat::test_that("build_reference_link_table() maps ids to link tables", {
  res <-
    build_reference_link_table()

  testthat::expect_equal(
    dplyr::pull(res, sel_column),
    c(
      "dataset_id",
      "data_source_id",
      "data_source_type_id",
      "sampling_method_id",
      "sample_id",
      "taxon_id",
      "trait_id",
      "abiotic_variable_id"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, sel_table),
    c(
      "DatasetReferences",
      "DatasetSourcesReference",
      "DatasetSourceTypeReference",
      "SamplingMethodReference",
      "SampleReference",
      "TaxonReference",
      "TraitsReference",
      "AbioticVariableReference"
    )
  )
})

testthat::test_that("build_reference_link_table() names existing tables", {
  vault_empty <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    )

  vec_tables <-
    DBI::dbListTables(
      conn = purrr::chuck(vault_empty, "db_con")
    )

  vec_link_tables <-
    build_reference_link_table() |>
    dplyr::pull(sel_table)

  testthat::expect_true(
    base::all(vec_link_tables %in% vec_tables)
  )
})
