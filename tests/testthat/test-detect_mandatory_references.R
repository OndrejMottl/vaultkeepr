testthat::test_that("detect_mandatory_references() is TRUE for all datasets", {
  vault_datasets <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    detect_mandatory_references(
      data_source = vault_datasets$data,
      sel_con = vault_datasets$db_con
    )

  testthat::expect_true(res)
})

testthat::test_that("detect_mandatory_references() is FALSE without one", {
  vault_datasets <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets()

  data_selection <-
    vault_datasets$data |>
    dplyr::filter(data_source_type_id %in% c(1L, 2L))

  res <-
    detect_mandatory_references(
      data_source = data_selection,
      sel_con = vault_datasets$db_con
    )

  testthat::expect_false(res)
})

testthat::test_that("detect_mandatory_references() reads abiotic variables", {
  vault_abiotic <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets() |>
    get_samples() |>
    get_abiotic_data(verbose = FALSE)

  # drop the source type, the other path to a mandatory reference in the
  #   fixture, so that only the abiotic variable can link to one
  vault_abiotic$data <-
    vault_abiotic$data |>
    dplyr::select(-data_source_type_id)

  vault_mandatory <-
    select_abiotic_var_by_name(
      con = vault_abiotic,
      sel_var_name = "soil_moisture"
    )

  vault_not_mandatory <-
    select_abiotic_var_by_name(
      con = vault_abiotic,
      sel_var_name = "temperature"
    )

  res_mandatory <-
    detect_mandatory_references(
      data_source = vault_mandatory$data,
      sel_con = vault_mandatory$db_con
    )

  res_not_mandatory <-
    detect_mandatory_references(
      data_source = vault_not_mandatory$data,
      sel_con = vault_not_mandatory$db_con
    )

  testthat::expect_true(res_mandatory)
  testthat::expect_false(res_not_mandatory)
})

testthat::test_that("detect_mandatory_references() is FALSE for no rows", {
  vault_datasets <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets()

  data_selection <-
    vault_datasets$data |>
    dplyr::filter(dataset_id < 0L)

  res <-
    detect_mandatory_references(
      data_source = data_selection,
      sel_con = vault_datasets$db_con
    )

  testthat::expect_false(res)
})

testthat::test_that("detect_mandatory_references() is FALSE without ids", {
  vault_empty <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    )

  res <-
    detect_mandatory_references(
      data_source = vault_empty$data,
      sel_con = vault_empty$db_con
    )

  testthat::expect_false(res)
})

testthat::test_that("build_mandatory_reference_query() is NULL without ids", {
  vault_empty <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    )

  res <-
    build_mandatory_reference_query(
      data_source = vault_empty$data,
      sel_con = vault_empty$db_con
    )

  testthat::expect_null(res)
})

testthat::test_that("build_mandatory_reference_query() is one lazy query", {
  vault_datasets <-
    open_vault(
      path = paste(tempdir(), "example.sqlite", sep = "/"),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    build_mandatory_reference_query(
      data_source = vault_datasets$data,
      sel_con = vault_datasets$db_con
    )

  sql_res <-
    dbplyr::sql_render(res) |>
    as.character()

  testthat::expect_s3_class(res, "tbl_sql")
  testthat::expect_match(sql_res, "LIMIT 1")
  testthat::expect_match(sql_res, "IN \\(SELECT")
  testthat::expect_no_match(sql_res, "IN \\(\\s*[0-9]")
  testthat::expect_equal(
    stringr::str_count(sql_res, "FROM `Datasets`"),
    1L
  )
})
