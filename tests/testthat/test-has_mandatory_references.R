testthat::test_that("has_mandatory_references() is TRUE for all datasets", {
  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  res <-
    has_mandatory_references(
      data_source = purrr::chuck(vault_datasets, "data"),
      sel_con = purrr::chuck(vault_datasets, "db_con")
    )

  testthat::expect_true(res)
})

testthat::test_that("has_mandatory_references() is FALSE without one", {
  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  data_selection <-
    purrr::chuck(vault_datasets, "data") |>
    dplyr::filter(data_source_type_id %in% c(1L, 2L))

  res <-
    has_mandatory_references(
      data_source = data_selection,
      sel_con = purrr::chuck(vault_datasets, "db_con")
    )

  testthat::expect_false(res)
})

testthat::test_that("has_mandatory_references() reads abiotic variables", {
  vault_abiotic <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets() |>
    get_samples() |>
    get_abiotic_data(verbose = FALSE)

  # Drop the source type, the other path to a mandatory reference in the
  #   fixture, so that only the abiotic variable can link to one
  data_abiotic_no_source_type <-
    purrr::chuck(vault_abiotic, "data") |>
    dplyr::select(-"data_source_type_id")

  vault_abiotic_no_source_type <-
    base::structure(
      .Data = base::list(
        data = data_abiotic_no_source_type,
        db_con = purrr::chuck(vault_abiotic, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  vault_mandatory <-
    select_abiotic_var_by_name(
      con = vault_abiotic_no_source_type,
      sel_var_name = "soil_moisture"
    )

  vault_not_mandatory <-
    select_abiotic_var_by_name(
      con = vault_abiotic_no_source_type,
      sel_var_name = "temperature"
    )

  res_mandatory <-
    has_mandatory_references(
      data_source = purrr::chuck(vault_mandatory, "data"),
      sel_con = purrr::chuck(vault_mandatory, "db_con")
    )

  res_not_mandatory <-
    has_mandatory_references(
      data_source = purrr::chuck(vault_not_mandatory, "data"),
      sel_con = purrr::chuck(vault_not_mandatory, "db_con")
    )

  testthat::expect_true(res_mandatory)
  testthat::expect_false(res_not_mandatory)
})

testthat::test_that("has_mandatory_references() is FALSE for no rows", {
  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  data_selection <-
    purrr::chuck(vault_datasets, "data") |>
    dplyr::filter(dataset_id < 0L)

  res <-
    has_mandatory_references(
      data_source = data_selection,
      sel_con = purrr::chuck(vault_datasets, "db_con")
    )

  testthat::expect_false(res)
})

testthat::test_that("has_mandatory_references() is FALSE without ids", {
  vault_empty <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    )

  res <-
    has_mandatory_references(
      data_source = purrr::chuck(vault_empty, "data"),
      sel_con = purrr::chuck(vault_empty, "db_con")
    )

  testthat::expect_false(res)
})
