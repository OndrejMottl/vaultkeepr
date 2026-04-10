testthat::test_that("get correct class", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")


  res <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus"
    )

  testthat::expect_s3_class(res, "tbl_sql")
})

testthat::test_that("get correct dataset structure", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")


  res <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus"
    )

  testthat::expect_equal(
    colnames(res),
    c("sample_id", "value", "taxon_id")
  )
})

testthat::test_that("get original names", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")


  res <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::pull(taxon_id)

  testthat::expect_true(is.numeric(res))
  testthat::expect_true(all(res <= 45))
})

testthat::test_that("get only genera", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")


  res <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus"
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::pull(taxon_id)

  testthat::expect_true(is.numeric(res))
  testthat::expect_true(all(res >= 46))
  testthat::expect_true(all(res < 55))
})

testthat::test_that("get only family", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")


  res <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "family"
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::pull(taxon_id)

  testthat::expect_true(is.numeric(res))
  testthat::expect_true(all(res >= 55))
  testthat::expect_true(all(res < 60))
})


# errors ----
testthat::test_that("error when data_source is not a SQL call", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  testthat::expect_error(
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa") %>%
        dplyr::collect(),
      sel_con = test_con,
      to = "genus"
    )
  )
})

testthat::test_that("error when sel_con is not a SQLiteConnection", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  testthat::expect_error(
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = "bad",
      to = "genus"
    )
  )
})

testthat::test_that("error when `to` is not a valid value", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  testthat::expect_error(
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "bad"
    )
  )
})


# classification_data argument ----
testthat::test_that("classification_data local tibble gives same result", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  data_class <-
    dplyr::tbl(test_con, "TaxonClassification") %>%
    dplyr::collect()

  res_db <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus"
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  res_local <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus",
      classification_data = data_class
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    dplyr::arrange(taxon_id) %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(res_local, res_db)
})

testthat::test_that("custom classification_data overrides mapping", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  # Map every species to genus 46 instead of their real genus
  data_class_custom <-
    dplyr::tbl(test_con, "TaxonClassification") %>%
    dplyr::collect() %>%
    dplyr::mutate(taxon_genus = 46L)

  res <-
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus",
      classification_data = data_class_custom
    ) %>%
    dplyr::distinct(taxon_id) %>%
    dplyr::collect() %>%
    dplyr::pull("taxon_id")

  testthat::expect_equal(res, 46L)
})

testthat::test_that("error when classification_data is not data.frame", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  testthat::expect_error(
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus",
      classification_data = "bad"
    )
  )
})

testthat::test_that("error when classification_data missing taxon_id", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  data_bad <-
    dplyr::tbl(test_con, "TaxonClassification") %>%
    dplyr::collect() %>%
    dplyr::select(-"taxon_id")

  testthat::expect_error(
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus",
      classification_data = data_bad
    )
  )
})

testthat::test_that("error when classification_data missing target col", {
  test_con <-
    open_vault(
      path = paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    ) %>%
    purrr::chuck("db_con")

  data_bad <-
    dplyr::tbl(test_con, "TaxonClassification") %>%
    dplyr::collect() %>%
    dplyr::select(-"taxon_genus")

  testthat::expect_error(
    classify_taxa(
      data_source = dplyr::tbl(test_con, "SampleTaxa"),
      sel_con = test_con,
      to = "genus",
      classification_data = data_bad
    )
  )
})
