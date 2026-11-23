# Set the random seed of the test database, so that the dummy data drawn
#   in helper_make_database.R are the same in every test run
apply_fixture_seed <- function(seed = c(0000 - 0002 - 9796 - 5081)) {
  res <-
    base::set.seed(seed = base::as.character(seed))

  return(base::invisible(res))
}
