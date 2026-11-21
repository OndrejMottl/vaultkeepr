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
    select_dataset_by_geo(verbose = FALSE)

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
    select_dataset_by_geo(verbose = FALSE)

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("basic longitude filter", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(
      sel_dataset_type = "vegetation_plot"
    ) %>%
    select_dataset_by_geo(
      long_lim = c(0, 180),
      sel_dataset_type = "vegetation_plot"
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(coord_long) %>%
    dplyr::collect() %>%
    purrr::chuck("coord_long")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values >= 0))
  testthat::expect_true(all(test_datasets_values <= 180))
})

testthat::test_that("basic latitude filter", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(
      sel_dataset_type = "vegetation_plot"
    ) %>%
    select_dataset_by_geo(
      lat_lim = c(0, -90),
      sel_dataset_type = "vegetation_plot"
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(coord_lat) %>%
    dplyr::collect() %>%
    purrr::chuck("coord_lat")

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values <= 0))
  testthat::expect_true(all(test_datasets_values >= -90))
})

testthat::test_that("basic lat/long filter", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(
      sel_dataset_type = "vegetation_plot"
    ) %>%
    select_dataset_by_geo(
      lat_lim = c(0, 90),
      long_lim = c(0, 90),
      sel_dataset_type = "vegetation_plot"
    )

  test_datasets_values <-
    test_datasets$data %>%
    dplyr::select(coord_lat, coord_long) %>%
    dplyr::collect() %>%
    unlist()

  testthat::expect_true(is.numeric(test_datasets_values))
  testthat::expect_true(all(test_datasets_values >= 0))
  testthat::expect_true(all(test_datasets_values <= 90))
})

testthat::test_that("only filter by sel_dataset_type", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    select_dataset_by_type(
      sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive")
    ) %>%
    select_dataset_by_geo(
      lat_lim = c(0, 90),
      long_lim = c(0, 90),
      sel_dataset_type = "vegetation_plot",
      verbose = FALSE
    )

  data_filter_vegetation <-
    test_datasets$data %>%
    dplyr::filter(dataset_type == "vegetation_plot") %>%
    dplyr::select(coord_lat, coord_long) %>%
    dplyr::collect() %>%
    unlist()

  data_filter_fossil <-
    test_datasets$data %>%
    dplyr::filter(dataset_type == "fossil_pollen_archive") %>%
    dplyr::select(coord_lat, coord_long) %>%
    dplyr::collect() %>%
    unlist()

  testthat::expect_true(all(data_filter_vegetation >= 0))
  testthat::expect_true(all(data_filter_vegetation <= 90))

  testthat::expect_false(all(data_filter_fossil >= 0))
  testthat::expect_false(all(data_filter_fossil <= 90))
})

testthat::test_that("get a message when sel_dataset_type differ", {
  test_f <- function() {
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_type(
        sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive")
      ) %>%
      select_dataset_by_geo(
        sel_dataset_type = "vegetation_plot",
        verbose = TRUE
      )
  }

  quiet_f <-
    purrr::quietly(test_f)

  res <-
    quiet_f()

  testthat::expect_true(length(res$messages) > 0)
})

# Box applies to every dataset type by default -----
testthat::test_that("default box keeps only datasets inside it", {
  con_vault <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      )
    )

  data_datasets <-
    con_vault |>
    get_datasets() |>
    select_dataset_by_geo(
      long_lim = c(0, 90),
      lat_lim = c(0, 90),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dplyr::collect()

  vec_dataset_id_expected <-
    dplyr::tbl(
      src = purrr::chuck(con_vault, "db_con"),
      from = "Datasets"
    ) |>
    dplyr::collect() |>
    dplyr::filter(
      coord_long >= 0,
      coord_long <= 90,
      coord_lat >= 0,
      coord_lat <= 90
    ) |>
    dplyr::pull(dataset_id)

  testthat::expect_setequal(
    dplyr::pull(data_datasets, dataset_id),
    vec_dataset_id_expected
  )

  testthat::expect_setequal(
    dplyr::pull(data_datasets, dataset_type),
    c("vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints")
  )
})

testthat::test_that("default box does not query the dataset types present", {
  testthat::local_mocked_bindings(
    get_common_dataset_type = function(...) {
      base::stop("get_common_dataset_type() is not expected")
    }
  )

  testthat::expect_no_error(
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      )
    ) |>
      get_datasets() |>
      select_dataset_by_geo(
        long_lim = c(0, 90),
        lat_lim = c(0, 90),
        verbose = FALSE
      )
  )
})

testthat::test_that("types left out of sel_dataset_type pass the box", {
  con_vault <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      )
    )

  data_datasets <-
    con_vault |>
    get_datasets() |>
    select_dataset_by_geo(
      long_lim = c(0, 90),
      lat_lim = c(0, 90),
      sel_dataset_type = c(
        "vegetation_plot",
        "fossil_pollen_archive",
        "gridpoints"
      ),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dplyr::collect()

  vec_trait_id_expected <-
    con_vault |>
    get_datasets() |>
    purrr::chuck("data") |>
    dplyr::filter(dataset_type == "traits") |>
    dplyr::collect() |>
    dplyr::pull(dataset_id)

  vec_trait_id <-
    data_datasets |>
    dplyr::filter(dataset_type == "traits") |>
    dplyr::pull(dataset_id)

  data_boxed <-
    data_datasets |>
    dplyr::filter(dataset_type != "traits")

  vec_long <-
    dplyr::pull(data_boxed, coord_long)

  vec_lat <-
    dplyr::pull(data_boxed, coord_lat)

  testthat::expect_setequal(vec_trait_id, vec_trait_id_expected)

  testthat::expect_true(
    base::all(
      vec_long >= 0 & vec_long <= 90 & vec_lat >= 0 & vec_lat <= 90
    )
  )
})

# Box limits and missing coordinates -----
testthat::test_that("box limits are inclusive and missing coordinates drop", {
  con_fixture <-
    build_geo_box_fixture()

  withr::defer(
    expr = DBI::dbDisconnect(
      conn = purrr::chuck(con_fixture, "db_con")
    )
  )

  vec_dataset_id <-
    con_fixture |>
    select_dataset_by_geo(
      long_lim = c(0, 90),
      lat_lim = c(0, 90),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dplyr::collect() |>
    dplyr::pull(dataset_id)

  testthat::expect_equal(
    base::sort(vec_dataset_id),
    c(1L, 2L, 3L, 8L)
  )
})

testthat::test_that("box limits may be given in either order", {
  con_fixture <-
    build_geo_box_fixture()

  withr::defer(
    expr = DBI::dbDisconnect(
      conn = purrr::chuck(con_fixture, "db_con")
    )
  )

  vec_dataset_id <-
    con_fixture |>
    select_dataset_by_geo(
      long_lim = c(90, 0),
      lat_lim = c(90, 0),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dplyr::collect() |>
    dplyr::pull(dataset_id)

  testthat::expect_equal(
    base::sort(vec_dataset_id),
    c(1L, 2L, 3L, 8L)
  )
})

testthat::test_that("types left out pass with any or missing coordinates", {
  con_fixture <-
    build_geo_box_fixture()

  withr::defer(
    expr = DBI::dbDisconnect(
      conn = purrr::chuck(con_fixture, "db_con")
    )
  )

  vec_dataset_id <-
    con_fixture |>
    select_dataset_by_geo(
      long_lim = c(0, 90),
      lat_lim = c(0, 90),
      sel_dataset_type = c(
        "vegetation_plot",
        "fossil_pollen_archive",
        "gridpoints"
      ),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dplyr::collect() |>
    dplyr::pull(dataset_id)

  testthat::expect_equal(
    base::sort(vec_dataset_id),
    c(1L, 2L, 3L, 4L, 7L, 8L)
  )
})

# Query shape -----
testthat::test_that("default box sends range conditions served by the index", {
  con_vault <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      )
    )

  sql_box <-
    con_vault |>
    get_datasets() |>
    select_dataset_by_geo(
      long_lim = c(0, 90),
      lat_lim = c(0, 90),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dbplyr::sql_render()

  vec_plan_detail <-
    DBI::dbGetQuery(
      conn = purrr::chuck(con_vault, "db_con"),
      statement = stringr::str_c("EXPLAIN QUERY PLAN ", sql_box)
    ) |>
    dplyr::pull(detail)

  testthat::expect_no_match(sql_box, "CASE")
  testthat::expect_match(sql_box, "`coord_long` >= 0")
  testthat::expect_true(
    base::any(
      stringr::str_detect(
        string = vec_plan_detail,
        pattern = "idx_datasets_coord_long_lat"
      )
    )
  )
})

testthat::test_that("pass-through of traits keeps range conditions and index", {
  con_vault <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      )
    )

  sql_box <-
    con_vault |>
    get_datasets() |>
    select_dataset_by_geo(
      long_lim = c(0, 90),
      lat_lim = c(0, 90),
      sel_dataset_type = c(
        "vegetation_plot",
        "fossil_pollen_archive",
        "gridpoints"
      ),
      verbose = FALSE
    ) |>
    purrr::chuck("data") |>
    dbplyr::sql_render()

  vec_plan_detail <-
    DBI::dbGetQuery(
      conn = purrr::chuck(con_vault, "db_con"),
      statement = stringr::str_c("EXPLAIN QUERY PLAN ", sql_box)
    ) |>
    dplyr::pull(detail)

  testthat::expect_no_match(sql_box, "CASE")
  testthat::expect_match(sql_box, "`coord_long` >= 0")
  testthat::expect_true(
    base::any(
      stringr::str_detect(
        string = vec_plan_detail,
        pattern = "idx_datasets_coord_long_lat"
      )
    )
  )
})

# errors ----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    select_dataset_by_geo()
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
      select_dataset_by_geo()
  )
})

testthat::test_that("error without `dataset_type_id`", {
  con_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      )
    ) |>
    get_datasets()

  data_no_type_id <-
    purrr::chuck(con_datasets, "data") |>
    dplyr::select(-"dataset_type_id")

  con_no_type_id <-
    base::structure(
      .Data = base::list(
        data = data_no_type_id,
        db_con = purrr::chuck(con_datasets, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  testthat::expect_error(
    select_dataset_by_geo(
      con = con_no_type_id,
      verbose = FALSE
    )
  )

  testthat::expect_message(
    base::try(
      expr = select_dataset_by_geo(
        con = con_no_type_id,
        verbose = TRUE
      ),
      silent = TRUE
    ),
    regexp = "dataset_type_id"
  )
})

testthat::test_that("error with bad `sel_dataset_type` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_geo(
        sel_dataset_type = 123,
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `sel_dataset_type`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_geo(
        sel_dataset_type = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `long_lim` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_geo(
        long_lim = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `lat_lim` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_geo(
        lat_lim = "bad",
        verbose = FALSE
      )
  )
})

testthat::test_that("error with bad `verbose` class", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      select_dataset_by_geo(
        verbose = 123
      )
  )
})
