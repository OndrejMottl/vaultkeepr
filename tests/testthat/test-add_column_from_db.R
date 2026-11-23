# Output -----
testthat::test_that("add_column_from_db() replaces the id with the name", {
  con_lookup <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_lookup)
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "TraitsDomain",
    value = tibble::tibble(
      trait_domain_id = c(1L, 2L),
      trait_domain_name = c("Leaf area", "Seed mass"),
      trait_unit = c("mm2", "mg")
    )
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "Values",
    value = tibble::tibble(
      trait_domain_id = c(1L, 2L, 2L),
      trait_value = c(10, 20, 30)
    )
  )

  res <-
    add_column_from_db(
      con = con_lookup,
      data_to_rename = dplyr::tbl(src = con_lookup, from = "Values"),
      sel_column_id = "trait_domain_id",
      sel_column_name = "trait_domain_name",
      table_name = "TraitsDomain"
    ) |>
    dplyr::collect()

  testthat::expect_named(
    res,
    c("trait_domain_name", "trait_value", "trait_unit")
  )
  testthat::expect_equal(
    dplyr::pull(res, "trait_domain_name"),
    c("Leaf area", "Seed mass", "Seed mass")
  )
})


# Functionality -----
testthat::test_that("add_column_from_db() keeps columns the data have", {
  con_lookup <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_lookup)
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "TraitsDomain",
    value = tibble::tibble(
      trait_domain_id = c(1L, 2L),
      trait_domain_name = c("Leaf area", "Seed mass"),
      trait_unit = c("mm2", "mg")
    )
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "Values",
    value = tibble::tibble(
      trait_domain_id = c(1L, 2L),
      trait_unit = c("cm2", "g"),
      trait_value = c(10, 20)
    )
  )

  res <-
    add_column_from_db(
      con = con_lookup,
      data_to_rename = dplyr::tbl(src = con_lookup, from = "Values"),
      sel_column_id = "trait_domain_id",
      sel_column_name = "trait_domain_name",
      table_name = "TraitsDomain"
    ) |>
    dplyr::collect()

  testthat::expect_named(
    res,
    c("trait_domain_name", "trait_unit", "trait_value")
  )
  testthat::expect_equal(
    dplyr::pull(res, "trait_unit"),
    c("cm2", "g")
  )
})

testthat::test_that("add_column_from_db() drops unmatched rows if inner", {
  con_lookup <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_lookup)
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "Taxa",
    value = tibble::tibble(
      taxon_id = c(1L, 2L),
      taxon_name = c("Betula", "Alnus")
    )
  )

  data_values <-
    tibble::tibble(
      taxon_id = c(1L, 3L),
      value = c(5, 7)
    )

  res_left <-
    add_column_from_db(
      con = con_lookup,
      data_to_rename = dplyr::copy_to(
        dest = con_lookup,
        df = data_values,
        name = "values_left"
      ),
      sel_column_id = "taxon_id",
      sel_column_name = "taxon_name",
      table_name = "Taxa",
      join_type = "left_join"
    ) |>
    dplyr::collect()

  res_inner <-
    add_column_from_db(
      con = con_lookup,
      data_to_rename = dplyr::copy_to(
        dest = con_lookup,
        df = data_values,
        name = "values_inner"
      ),
      sel_column_id = "taxon_id",
      sel_column_name = "taxon_name",
      table_name = "Taxa",
      join_type = "inner_join"
    ) |>
    dplyr::collect()

  testthat::expect_equal(base::nrow(res_left), 2L)
  testthat::expect_equal(dplyr::pull(res_inner, "taxon_name"), "Betula")
})

testthat::test_that("add_column_from_db() joins sel_table if given", {
  con_lookup <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_lookup)
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "Samples",
    value = tibble::tibble(
      sample_id = c(1L, 2L),
      sample_name = c("plot_a", "plot_b"),
      age = c(0, 100)
    )
  )

  DBI::dbWriteTable(
    conn = con_lookup,
    name = "Links",
    value = tibble::tibble(
      sample_id_link = c(2L, 1L)
    )
  )

  res <-
    add_column_from_db(
      con = con_lookup,
      data_to_rename = dplyr::tbl(src = con_lookup, from = "Links"),
      sel_column_id = "sample_id_link",
      sel_column_name = "sample_name",
      sel_table = dplyr::tbl(src = con_lookup, from = "Samples") |>
        dplyr::select("sample_id", "sample_name"),
      sel_join_by = c("sample_id_link" = "sample_id")
    ) |>
    dplyr::collect()

  testthat::expect_named(res, "sample_name")
  testthat::expect_equal(
    dplyr::pull(res, "sample_name"),
    c("plot_b", "plot_a")
  )
})


# Edge cases -----
testthat::test_that("add_column_from_db() needs the id column", {
  data_values <-
    tibble::tibble(value = c(1, 2))

  res <-
    add_column_from_db(
      con = NULL,
      data_to_rename = data_values,
      sel_column_id = "taxon_id",
      sel_column_name = "taxon_name",
      table_name = "Taxa"
    )

  testthat::expect_identical(res, data_values)
})

testthat::test_that("add_column_from_db() keeps an existing name column", {
  data_values <-
    tibble::tibble(
      taxon_id = c(1L, 2L),
      taxon_name = c("Betula", "Alnus"),
      value = c(1, 2)
    )

  res <-
    add_column_from_db(
      con = NULL,
      data_to_rename = data_values,
      sel_column_id = "taxon_id",
      sel_column_name = "taxon_name",
      table_name = "Taxa"
    )

  testthat::expect_identical(
    res,
    dplyr::select(data_values, "taxon_name", "value")
  )
})
