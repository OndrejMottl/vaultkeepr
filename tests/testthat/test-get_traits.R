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
    get_samples() %>%
    get_traits()

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
    get_samples() %>%
    get_traits()

  testthat::expect_s3_class(test_datasets$data, "tbl_sql")
})

testthat::test_that("number of trait ids", {
  test_datasets <-
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

  test_n_trait_id <-
    test_datasets$data %>%
    dplyr::distinct(trait_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_n_trait_id <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TraitsValue"),
      by = c("sample_id")
    ) %>%
    dplyr::distinct(trait_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::count(name = "N") %>%
    dplyr::pull("N")

  testthat::expect_equal(test_n_trait_id, actual_n_trait_id)

  DBI::dbDisconnect(con_db)
})

testthat::test_that("size of a total dataset", {
  test_datasets <-
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

  test_n_datasets <-
    test_datasets$data %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_n_datasets <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TraitsValue"),
      by = "sample_id"
    ) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  testthat::expect_equal(test_n_datasets, actual_n_datasets)

  DBI::dbDisconnect(con_db)
})

testthat::test_that("filtering the trait data by taxa data", {
  test_datasets_with_taxa <-
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
    select_taxa_by_name("taxon_1") %>%
    get_traits(verbose = FALSE) %>%
    purrr::chuck("data") %>%
    dplyr::collect()

  testthat::expect_true(nrow(test_datasets_with_taxa) > 0)
  testthat::expect_true("taxon_name" %in% colnames(test_datasets_with_taxa))
  testthat::expect_equal(
    test_datasets_with_taxa %>%
      dplyr::distinct(taxon_name) %>%
      dplyr::pull("taxon_name"),
    "taxon_1"
  )
})

testthat::test_that("filtering the trait data by taxa-classified ", {
  test_datasets_with_taxa <-
    suppressWarnings(
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_taxa(
        classify_to = "family"
      ) %>%
      get_traits(
        classify_to = "family",
        verbose = FALSE
      )
    ) %>%
    purrr::chuck("data") %>%
    dplyr::collect()

  test_vec_taxon_id <-
    test_datasets_with_taxa %>%
    dplyr::distinct(taxon_id) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  actual_vec_taxon_id <-
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets"),
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "SampleTaxa"),
      by = "sample_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TaxonClassification"),
      by = "taxon_id"
    ) %>%
    dplyr::select(-"taxon_id") %>%
    dplyr::rename(
      taxon_id = "taxon_family"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "TraitsValue") %>%
        dplyr::left_join(
          dplyr::tbl(con_db, "TaxonClassification"),
          by = "taxon_id"
        ) %>%
        dplyr::select(-"taxon_id") %>%
        dplyr::rename(
          taxon_id = "taxon_family"
        ),
      by = c("sample_id", "taxon_id")
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  testthat::expect_true(nrow(test_datasets_with_taxa) > 0)
  testthat::expect_true("taxon_id" %in% colnames(test_datasets_with_taxa))
  testthat::expect_equal(
    test_vec_taxon_id,
    actual_vec_taxon_id
  )

  DBI::dbDisconnect(con_db)
})

testthat::test_that("get a message when adding taxa ", {
  test_f <- function() {
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
      get_traits(verbose = TRUE)
  }

  quiet_f <-
    purrr::quietly(test_f)

  res <-
    quiet_f()

  testthat::expect_true(length(res$messages) > 0)
})

# errors -----
testthat::test_that("error wihtout `open_vault()`", {
  testthat::expect_error(
    get_traits()
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
      get_traits()
  )
})

testthat::test_that("error wihtout `get_samples()`", {
  testthat::expect_error(
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
      get_datasets() %>%
      get_traits()
  )
})

testthat::test_that("error with bad `classify_to` class", {
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
      get_traits(
        classify_to = 123
      )
  )
})

testthat::test_that("error with bad `classify_to` value", {
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
      get_traits(
        classify_to = "pikachu"
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
      get_samples() %>%
      get_traits(
        verbose = 123
      )
  )
})


# classification_data argument ----
testthat::test_that("classification_data tibble gives same genus result", {
  test_con_orig <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa(classify_to = "original")

  data_class <-
    get_classification_table(
      con = test_con_orig,
      return_raw_data = TRUE
    )

  res_with_data <-
    suppressWarnings(
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_traits(
        classify_to = "genus",
        verbose = FALSE,
        classification_data = data_class
      )
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  res_without_data <-
    suppressWarnings(
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_traits(
        classify_to = "genus",
        verbose = FALSE
      )
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(res_with_data, res_without_data)
})

testthat::test_that("custom classification_data overrides genus mapping", {
  test_con_orig <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    get_datasets() %>%
    get_samples() %>%
    get_taxa(classify_to = "original")

  data_class_custom <-
    get_classification_table(
      con = test_con_orig,
      return_raw_data = TRUE
    ) %>%
    dplyr::mutate(taxon_genus = 46L)

  res <-
    suppressWarnings(
      open_vault(
        path = paste(
          tempdir(),
          "example.sqlite",
          sep = "/"
        )
      ) %>%
      get_datasets() %>%
      get_samples() %>%
      get_traits(
        classify_to = "genus",
        verbose = FALSE,
        classification_data = data_class_custom
      )
    ) %>%
    purrr::chuck("data") %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    tidyr::drop_na() %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(res, 46L)
})

testthat::test_that("error when classification_data is not data.frame", {
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
      get_traits(
        classify_to = "genus",
        verbose = FALSE,
        classification_data = "bad"
      )
  )
})

# Classification of the selected samples -----

testthat::test_that("get_traits() narrows trait values to the selection", {
  vault_selected <-
    build_classification_fixture(vec_dataset_type = "traits")

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_traits <-
    base::suppressWarnings(
      get_traits(
        con = vault_selected,
        classify_to = "genus",
        verbose = FALSE
      )
    ) |>
    purrr::chuck("data") |>
    dbplyr::sql_render() |>
    base::as.character()

  testthat::expect_match(
    sql_traits,
    "`sample_id` IN \\(SELECT DISTINCT `sample_id`"
  )
})

testthat::test_that("get_traits() classifies only the selected values", {
  vault_selected <-
    build_classification_fixture(vec_dataset_type = "traits")

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_traits <-
    base::suppressWarnings(
      get_traits(
        con = vault_selected,
        classify_to = "genus",
        verbose = FALSE
      )
    ) |>
    purrr::chuck("data") |>
    dbplyr::sql_render()

  vec_plan_detail <-
    DBI::dbGetQuery(
      conn = purrr::chuck(vault_selected, "db_con"),
      statement = stringr::str_c("EXPLAIN QUERY PLAN ", sql_traits)
    ) |>
    dplyr::pull(var = "detail")

  # A scan of the classification table means the taxa of the whole
  #   database are classified before the selection applies
  testthat::expect_false(
    base::any(
      stringr::str_detect(
        string = vec_plan_detail,
        pattern = "SCAN TaxonClassification"
      )
    )
  )
  testthat::expect_true(
    base::any(
      stringr::str_detect(
        string = vec_plan_detail,
        pattern = stringr::str_c(
          "^SEARCH TraitsValue USING (COVERING )?INDEX ",
          "idx_traitsvalue_sample_id"
        )
      )
    )
  )
})

testthat::test_that("get_traits() keeps the plain join for original taxa", {
  vault_selected <-
    build_classification_fixture(vec_dataset_type = "traits")

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sql_traits <-
    base::suppressWarnings(
      get_traits(
        con = vault_selected,
        classify_to = "original",
        verbose = FALSE
      )
    ) |>
    purrr::chuck("data") |>
    dbplyr::sql_render() |>
    base::as.character()

  testthat::expect_no_match(sql_traits, "IN \\(SELECT DISTINCT")
})

testthat::test_that("get_traits() classifies the selection as before", {
  vault_selected <-
    build_classification_fixture(vec_dataset_type = "traits")

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  sel_con <-
    purrr::chuck(vault_selected, "db_con")

  data_selected <-
    purrr::chuck(vault_selected, "data") |>
    dplyr::collect()

  data_trait_values <-
    get_trait_values_by_version(db_con = sel_con) |>
    dplyr::collect()

  data_classification <-
    dplyr::tbl(
      src = sel_con,
      from = "TaxonClassification"
    ) |>
    dplyr::collect()

  vec_rank_column <-
    c(
      original = "taxon_id",
      species = "taxon_species",
      genus = "taxon_genus",
      family = "taxon_family"
    )

  purrr::iwalk(
    .x = vec_rank_column,
    .f = ~ {
      data_res <-
        base::suppressWarnings(
          get_traits(
            con = vault_selected,
            classify_to = .y,
            verbose = FALSE
          )
        ) |>
        purrr::chuck("data") |>
        dplyr::collect() |>
        dplyr::arrange(dplyr::across(dplyr::everything()))

      data_expected <-
        build_expected_classified_values(
          data_selected = data_selected,
          data_values = data_trait_values,
          data_classification = data_classification,
          sel_rank_column = .x,
          vec_join_column = c("dataset_id", "sample_id"),
          sel_suffix = "_trait"
        ) |>
        dplyr::arrange(dplyr::across(dplyr::everything()))

      testthat::expect_equal(data_res, data_expected)
    }
  )
})

testthat::test_that("get_traits() classifies an empty selection", {
  vault_selected <-
    build_classification_fixture(vec_dataset_type = "traits")

  withr::defer(
    DBI::dbDisconnect(
      conn = purrr::chuck(vault_selected, "db_con")
    )
  )

  data_samples_none <-
    purrr::chuck(vault_selected, "data") |>
    dplyr::filter(
      .data[["sample_id"]] < 0L
    )

  vault_empty <-
    base::structure(
      base::list(
        data = data_samples_none,
        db_con = purrr::chuck(vault_selected, "db_con")
      ),
      class = c("list", "vault_pipe")
    )

  data_res_empty <-
    base::suppressWarnings(
      get_traits(
        con = vault_empty,
        classify_to = "genus",
        verbose = FALSE
      )
    ) |>
    purrr::chuck("data") |>
    dplyr::collect()

  vec_column_selected <-
    base::suppressWarnings(
      get_traits(
        con = vault_selected,
        classify_to = "genus",
        verbose = FALSE
      )
    ) |>
    purrr::chuck("data") |>
    base::colnames()

  testthat::expect_equal(base::nrow(data_res_empty), 0L)
  testthat::expect_equal(base::colnames(data_res_empty), vec_column_selected)
})
