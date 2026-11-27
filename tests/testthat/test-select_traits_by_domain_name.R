testthat::test_that("return correct class-high", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name("Plant heigh")

  testthat::expect_s3_class(test_datasets, "vault_pipe")
})

testthat::test_that("basic data.frame structure", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name("Plant heigh")

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("get only correct trait domain", {
  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name("Plant heigh") %>%
    purrr::chuck("data") %>%
    dplyr::distinct(trait_domain_name) %>%
    dplyr::collect() %>%
    purrr::chuck("trait_domain_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 1)
  testthat::expect_equal(test_datasets_values, "Plant heigh")
})

testthat::test_that("get correct taxa for multi-domain selection", {
  test_datasets_values <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    select_traits_by_domain_name(
      sel_domain = c("Stem specific density", "Diaspore mass")
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(trait_domain_name) %>%
    dplyr::collect() %>%
    purrr::chuck("trait_domain_name")

  testthat::expect_true(is.character(test_datasets_values))
  testthat::expect_true(length(test_datasets_values) == 2)
  testthat::expect_equal(
    test_datasets_values,
    c("Stem specific density", "Diaspore mass")
  )
})

testthat::test_that("subset dataset", {
  test_datasets_full <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits()

  test_datasets_sub <-
    test_datasets_full %>%
    select_traits_by_domain_name("Plant heigh")

  nrows_full <-
    test_datasets_full$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  nrows_sub <-
    test_datasets_sub$data %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_true(is.numeric(nrows_full))
  testthat::expect_true(is.numeric(nrows_sub))
  testthat::expect_true(nrows_full > 0)
  testthat::expect_true(nrows_sub > 0)
  testthat::expect_true(nrows_sub < nrows_full)
})

testthat::test_that("return empty dataset", {
  nrows_empty <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type("traits") %>%
    get_samples() %>%
    get_traits() %>%
    # select a domain that does not exist
    select_traits_by_domain_name("pikachu") %>%
    purrr::chuck("data") %>%
    dplyr::count() %>%
    dplyr::pull(n)

  testthat::expect_equal(nrows_empty, 0)
})

# Trait units -----
testthat::test_that("select_traits_by_domain_name() keeps one trait_unit", {
  vec_column_names <-
    open_vault(
      path = base::file.path(base::tempdir(), "example.sqlite"),
      verbose = FALSE
    ) |>
    get_datasets() |>
    select_dataset_by_type(sel_dataset_type = "traits") |>
    get_samples() |>
    get_traits(verbose = FALSE) |>
    select_traits_by_domain_name(sel_domain = "Plant heigh") |>
    purrr::chuck("data") |>
    base::colnames()

  testthat::expect_equal(base::sum(vec_column_names == "trait_unit"), 1L)
  testthat::expect_false(
    base::any(c("trait_unit.x", "trait_unit.y") %in% vec_column_names)
  )
})

testthat::test_that("select_traits_by_domain_name() keeps the domain units", {
  vec_domain <-
    c("Stem specific density", "Diaspore mass")

  path_database <-
    base::file.path(base::tempdir(), "example.sqlite")

  data_units <-
    open_vault(
      path = path_database,
      verbose = FALSE
    ) |>
    get_datasets() |>
    select_dataset_by_type(sel_dataset_type = "traits") |>
    get_samples() |>
    get_traits(verbose = FALSE) |>
    select_traits_by_domain_name(sel_domain = vec_domain) |>
    purrr::chuck("data") |>
    dplyr::select(
      dplyr::all_of(c("trait_domain_name", "trait_unit"))
    ) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::arrange(.data[["trait_domain_name"]])

  con_database <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_database
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_database)
  )

  data_expected <-
    dplyr::tbl(con_database, "TraitsDomain") |>
    dplyr::filter(.data[["trait_domain_name"]] %in% vec_domain) |>
    dplyr::select(
      dplyr::all_of(c("trait_domain_name", "trait_unit"))
    ) |>
    dplyr::collect() |>
    dplyr::arrange(.data[["trait_domain_name"]])

  testthat::expect_equal(base::nrow(data_units), 2L)
  testthat::expect_equal(data_units, data_expected)
})

testthat::test_that("select_traits_by_domain_name() keeps VegVault 2 units", {
  path_v2 <-
    build_trait_version_fixture(sel_version = "2.0.0")

  withr::defer(
    base::unlink(x = path_v2)
  )

  vault_v2 <-
    open_vault(
      path = path_v2,
      verbose = FALSE
    )

  withr::defer(
    DBI::dbDisconnect(conn = purrr::chuck(vault_v2, "db_con"))
  )

  # The trait values with their unit, as get_traits() reads them, in a
  #   trait dataset
  data_values <-
    get_trait_values_by_version(
      db_con = purrr::chuck(vault_v2, "db_con")
    ) |>
    dplyr::mutate(dataset_type = "traits")

  vault_values <-
    base::structure(
      base::list(
        data = data_values,
        db_con = purrr::chuck(vault_v2, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  data_res <-
    select_traits_by_domain_name(
      con = vault_values,
      sel_domain = "Leaf Area"
    ) |>
    purrr::chuck("data") |>
    dplyr::collect()

  testthat::expect_equal(
    base::colnames(data_res),
    c(
      base::colnames(data_values),
      "trait_domain_id",
      "trait_domain_name",
      "trait_domanin_description",
      "trait_name"
    )
  )
  testthat::expect_equal(dplyr::pull(data_res, "trait_unit"), "mm2")
})

testthat::test_that("select_traits_by_domain_name() keeps VegVault 1 as is", {
  path_v1 <-
    build_trait_version_fixture(sel_version = "1.0.0")

  withr::defer(
    base::unlink(x = path_v1)
  )

  vault_v1 <-
    open_vault(
      path = path_v1,
      verbose = FALSE
    )

  withr::defer(
    DBI::dbDisconnect(conn = purrr::chuck(vault_v1, "db_con"))
  )

  # The trait values as get_traits() reads them, in a trait dataset
  data_values <-
    get_trait_values_by_version(
      db_con = purrr::chuck(vault_v1, "db_con")
    ) |>
    dplyr::mutate(dataset_type = "traits")

  vault_values <-
    base::structure(
      base::list(
        data = data_values,
        db_con = purrr::chuck(vault_v1, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  data_res <-
    select_traits_by_domain_name(
      con = vault_values,
      sel_domain = "Leaf Area"
    ) |>
    purrr::chuck("data") |>
    dplyr::collect()

  testthat::expect_equal(
    base::colnames(data_res),
    c(
      base::colnames(data_values),
      "trait_domain_id",
      "trait_domain_name",
      "trait_domanin_description",
      "trait_name"
    )
  )
  testthat::expect_equal(
    dplyr::pull(data_res, "trait_domain_name"),
    "Leaf Area"
  )
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_traits_by_domain_name()
  )
})

testthat::test_that("error wihtout `get_datasets()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      select_traits_by_domain_name()
  )
})

testthat::test_that("error without `get_samples()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_traits_by_domain_name()
  )
})

testthat::test_that("error without `get_traits()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_samples() %>%
      select_traits_by_domain_name()
  )
})

testthat::test_that("error with bad `sel_domain` class", {
  testthat::expect_error(
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
      select_traits_by_domain_name(
        sel_domain = 123
      )
  )
})
