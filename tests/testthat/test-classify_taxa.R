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
