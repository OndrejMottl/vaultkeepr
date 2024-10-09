prepare_data <- function(
    data_source = NULL,
    focus = c("vegetation", "traits"),
    verbose = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "The 'data_source' must be a data.frame"
  )

  assertthat::assert_that(
    is.character(focus),
    msg = "The 'focus' must be a character"
  )

  focus <- match.arg(focus)

  assertthat::assert_that(
    focus %in% c("vegetation", "traits"),
    msg = "The 'focus' must be one of the following: 'vegetation', 'traits'"
  )

  assertthat::assert_that(
    is.logical(verbose),
    msg = "The 'verbose' must be a logical"
  )

  
}
