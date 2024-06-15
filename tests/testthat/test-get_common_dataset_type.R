testthat::test_that("get basic subset", {
  res <-
    get_common_dataset_type(
      data_source = tibble::tibble(
        id = 1:10,
        dataset_type = c(
          rep("vegetation_plot", 5),
          rep("fossil_pollen_archive", 5)
        )
      ),
      vec_dataset_type = c("vegetation_plot"),
      verbose = FALSE
    )

  testthat::expect_true(is.character(res))
  testthat::expect_equal(length(res), 1)
  testthat::expect_equal(res, "vegetation_plot")
})

testthat::test_that("subset - vec longer", {
  res <-
    get_common_dataset_type(
      data_source = tibble::tibble(
        id = 1:10,
        dataset_type = c(
          rep("vegetation_plot", 5),
          rep("fossil_pollen_archive", 5)
        )
      ),
      vec_dataset_type = c(
        "vegetation_plot", "fossil_pollen_archive", "traits", "gridpoints"
      ),
      verbose = FALSE
    )

  testthat::expect_true(is.character(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_equal(res, c("vegetation_plot", "fossil_pollen_archive"))
})

testthat::test_that("subset - no overlap", {
  res <-
    get_common_dataset_type(
      data_source = tibble::tibble(
        id = 1:10,
        dataset_type = c(
          rep("vegetation_plot", 5),
          rep("fossil_pollen_archive", 5)
        )
      ),
      vec_dataset_type = c("traits", "gridpoints"),
      verbose = FALSE
    )

  testthat::expect_true(is.character(res))
  testthat::expect_equal(length(res), 0)
  testthat::expect_equal(res, character(0))
})

# errors ----
testthat::test_that("subset - wrong vec_dataset_type class", {
  testthat::expect_error(
    get_common_dataset_type(
      data_source = tibble::tibble(
        id = 1:10,
        dataset_type = c(
          rep("vegetation_plot", 5),
          rep("fossil_pollen_archive", 5)
        )
      ),
      vec_dataset_type = 1:10,
      verbose = FALSE
    )
  )
})

testthat::test_that("subset - wrong data_source class", {
  testthat::expect_error(
    get_common_dataset_type(
      data_source = 1:10,
      vec_dataset_type = c("vegetation_plot"),
      verbose = FALSE
    )
  )
})

testthat::test_that("subset - wrong verbose class", {
  testthat::expect_error(
    get_common_dataset_type(
      data_source = tibble::tibble(
        id = 1:10,
        dataset_type = c(
          rep("vegetation_plot", 5),
          rep("fossil_pollen_archive", 5)
        )
      ),
      vec_dataset_type = c("vegetation_plot"),
      verbose = 1:10
    )
  )
})
