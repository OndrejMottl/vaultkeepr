# Set the random seed of the test and vignette databases, so that their
#   dummy data are the same in every build. The same file is in
#   tests/testthat/ and vignettes/; test-helper_make_database.R checks that
#   the two copies match.
apply_fixture_seed <- function(seed = c(0000 - 0002 - 9796 - 5081)) {
  res <-
    base::set.seed(seed = base::as.character(seed))

  return(base::invisible(res))
}
