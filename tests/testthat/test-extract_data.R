testthat::test_that("return correct class", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(verbose = FALSE)

  testthat::expect_s3_class(test_datasets, "data.frame")
})

testthat::test_that("return empty data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(verbose = FALSE)

  testthat::expect_equal(nrow(test_datasets), 0)
})

testthat::test_that("return full data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data(verbose = FALSE)

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_datasets <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::collect()

  testthat::expect_equal(nrow(test_datasets), nrow(actual_datasets))

  testthat::expect_equal(
    test_datasets$dataset_name %>% unique() %>% length(),
    actual_datasets$dataset_name %>% unique() %>% length()
  )

  DBI::dbDisconnect(con_db)
})

testthat::test_that("return correct class", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )

  testthat::expect_s3_class(test_datasets, "data.frame")
})

testthat::test_that("return empty data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )

  testthat::expect_equal(nrow(test_datasets), 0)
})

testthat::test_that("return full data.frame", {
  test_datasets <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_datasets <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::collect()

  testthat::expect_equal(nrow(test_datasets), nrow(actual_datasets))

  DBI::dbDisconnect(con_db)
})

# Mandatory references -----
testthat::test_that("alert when the data has a mandatory reference", {
  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  testthat::expect_message(
    extract_data(
      con = vault_datasets,
      verbose = TRUE
    ),
    regexp = "mandatory references"
  )
})

testthat::test_that("no alert when the data has no mandatory reference", {
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

  vault_selection <-
    base::structure(
      .Data = base::list(
        data = data_selection,
        db_con = purrr::chuck(vault_datasets, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  testthat::expect_no_message(
    extract_data(
      con = vault_selection,
      verbose = TRUE
    ),
    message = "mandatory references"
  )
})

testthat::test_that("the alert comes from has_mandatory_references()", {
  testthat::local_mocked_bindings(
    has_mandatory_references = function(...) {
      TRUE
    }
  )

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

  vault_selection <-
    base::structure(
      .Data = base::list(
        data = data_selection,
        db_con = purrr::chuck(vault_datasets, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  testthat::expect_message(
    extract_data(
      con = vault_selection,
      verbose = TRUE
    ),
    regexp = "mandatory references"
  )
})

testthat::test_that("the check does not call get_references()", {
  testthat::local_mocked_bindings(
    get_references = function(...) {
      base::stop("get_references() was called")
    }
  )

  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  testthat::expect_no_error(
    base::suppressMessages(
      extract_data(
        con = vault_datasets,
        verbose = TRUE
      )
    )
  )
})

testthat::test_that("no reference query when `verbose = FALSE`", {
  testthat::local_mocked_bindings(
    get_references = function(...) {
      base::stop("get_references() was called")
    },
    has_mandatory_references = function(...) {
      base::stop("references checked")
    }
  )

  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  testthat::expect_no_error(
    extract_data(
      con = vault_datasets,
      verbose = FALSE
    )
  )
})

testthat::test_that("no reference query when the check is switched off", {
  testthat::local_mocked_bindings(
    get_references = function(...) {
      base::stop("get_references() was called")
    },
    has_mandatory_references = function(...) {
      base::stop("references checked")
    }
  )

  vault_datasets <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets()

  testthat::expect_no_error(
    base::suppressMessages(
      extract_data(
        con = vault_datasets,
        check_mandatory_references = FALSE,
        verbose = TRUE
      )
    )
  )
})

# Trait units -----
testthat::test_that("extract_data() keeps the unit after the domain filter", {
  vault_traits <-
    open_vault(
      path = base::file.path(base::tempdir(), "example.sqlite"),
      verbose = FALSE
    ) |>
    get_datasets() |>
    select_dataset_by_type(sel_dataset_type = "traits") |>
    get_samples() |>
    get_traits(verbose = FALSE) |>
    select_traits_by_domain_name(sel_domain = "Plant heigh")

  data_raw <-
    extract_data(
      con = vault_traits,
      return_raw_data = TRUE,
      verbose = FALSE
    )

  data_packed <-
    extract_data(
      con = vault_traits,
      return_raw_data = FALSE,
      verbose = FALSE
    )

  vec_flag_packed_unit <-
    dplyr::pull(data_packed, "data_traits") |>
    purrr::map_lgl(
      .f = ~ "trait_unit" %in% base::colnames(.x)
    )

  testthat::expect_true("trait_unit" %in% base::colnames(data_raw))
  testthat::expect_false(base::anyNA(dplyr::pull(data_raw, "trait_unit")))
  testthat::expect_true(base::length(vec_flag_packed_unit) > 0L)
  testthat::expect_true(base::all(vec_flag_packed_unit))
})

# errors -----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    extract_data(verbose = FALSE)
  )
})

testthat::test_that("error with bad `con` class", {
  testthat::expect_error(
    extract_data(
      con = 123,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `con`", {
  testthat::expect_error(
    extract_data(
      con = "bad",
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `return_raw_data`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      return_raw_data = "bad",
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `return_raw_data`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      return_raw_data = 123,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `check_mandatory_references`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      check_mandatory_references = "bad",
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `check_mandatory_references`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      check_mandatory_references = 123,
      verbose = FALSE
    )
  )
})

testthat::test_that("error with bad `verbose`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      verbose = "bad"
    )
  )
})

testthat::test_that("error with bad `verbose`", {
  testthat::expect_error(
    extract_data(
      con = open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ),
      verbose = 123
    )
  )
})
