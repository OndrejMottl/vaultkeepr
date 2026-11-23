# Inputs -----
testthat::test_that("validate_vegvault_version() checks the version input", {
  list_invalid <-
    list(
      number = 2,
      two = c("1.0.0", "2.0.0"),
      missing = NA_character_,
      empty = character()
    )

  purrr::iwalk(
    .x = list_invalid,
    .f = ~ testthat::expect_error(
      base::suppressMessages(
        validate_vegvault_version(database_version = .x)
      ),
      label = .y
    )
  )

  testthat::expect_message(
    base::try(
      validate_vegvault_version(database_version = 2),
      silent = TRUE
    ),
    regexp = "single version string"
  )
})

testthat::test_that("validate_vegvault_version() checks verbose", {
  testthat::expect_error(
    base::suppressMessages(
      validate_vegvault_version(
        database_version = "2.0.0",
        verbose = "yes"
      )
    )
  )
})


# Supported versions -----
testthat::test_that("validate_vegvault_version() accepts 1.x and 2.x", {
  vec_supported <-
    c("1.0.0", "1.5.2", "2.0.0", "2.10.0", "2.99.99")

  purrr::walk(
    .x = vec_supported,
    .f = ~ {
      testthat::expect_invisible(
        validate_vegvault_version(database_version = .x)
      )

      res <-
        validate_vegvault_version(database_version = .x)

      testthat::expect_equal(res, .x)
    }
  )
})


# Unsupported versions -----
testthat::test_that("validate_vegvault_version() stops for older versions", {
  vec_older <-
    c("0.0.4", "0.9.9")

  purrr::walk(
    .x = vec_older,
    .f = ~ {
      testthat::expect_error(
        base::suppressMessages(
          validate_vegvault_version(database_version = .x)
        ),
        label = .x
      )

      testthat::expect_message(
        base::try(
          validate_vegvault_version(database_version = .x),
          silent = TRUE
        ),
        regexp = "older than 1\\.0\\.0"
      )
    }
  )
})

testthat::test_that("validate_vegvault_version() stops for newer versions", {
  vec_newer <-
    c("3.0.0", "10.0.0")

  purrr::walk(
    .x = vec_newer,
    .f = ~ {
      testthat::expect_error(
        base::suppressMessages(
          validate_vegvault_version(database_version = .x)
        ),
        label = .x
      )

      testthat::expect_message(
        base::try(
          validate_vegvault_version(database_version = .x),
          silent = TRUE
        ),
        regexp = "Update vaultkeepr"
      )
    }
  )
})

testthat::test_that("validate_vegvault_version() stops for unreadable ones", {
  vec_unreadable <-
    c("2.0.0-dev", "unknown", "", " 2.0.0", "{2.0.0}")

  purrr::walk(
    .x = vec_unreadable,
    .f = ~ {
      testthat::expect_error(
        base::suppressMessages(
          validate_vegvault_version(database_version = .x)
        ),
        label = .x
      )

      testthat::expect_message(
        base::try(
          validate_vegvault_version(database_version = .x),
          silent = TRUE
        ),
        regexp = "cannot be read as a version number"
      )
    }
  )
})


# Messages -----
testthat::test_that("validate_vegvault_version() names the version", {
  testthat::expect_message(
    base::try(
      validate_vegvault_version(database_version = "3.0.0"),
      silent = TRUE
    ),
    regexp = "3\\.0\\.0"
  )

  testthat::expect_message(
    base::try(
      validate_vegvault_version(database_version = "{2.0.0}"),
      silent = TRUE
    ),
    regexp = "\\{2\\.0\\.0\\}"
  )
})

testthat::test_that("validate_vegvault_version() stops quietly if asked", {
  testthat::expect_error(
    validate_vegvault_version(
      database_version = "3.0.0",
      verbose = FALSE
    )
  )

  testthat::expect_no_message(
    base::try(
      validate_vegvault_version(
        database_version = "3.0.0",
        verbose = FALSE
      ),
      silent = TRUE
    )
  )
})
