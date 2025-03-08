#' @title Extract data from a plan
#' @description Extract data from a vault connection. The data is first
#' processed with columns translated into more redable variants.
#' The data is then packed into a single tibble with nested data.frames,
#' containing information about:
#' - `data_samples` for sample metadata
#' - `data_samples_community` for community (vegetation) data
#' - `data_traits` for trait data
#' - `data_abiotic` for abiotic data
#' @param con A vault connection
#' @param return_raw_data A logical indicating whether to return raw data or
#' without any processing. Default is `FALSE`.
#' @param check_mandatory_references A logical indicating whether to check for
#' presentce of mandatory reference. Default is `TRUE`.
#' @param verbose A logical indicating whether output additional information.
#'  Default is `TRUE`.
#' @return A data.frame
#' @export
extract_data <- function(
    con,
    return_raw_data = FALSE,
    check_mandatory_references = TRUE,
    verbose = TRUE) {
  assertthat::assert_that(
    inherits(con, "vault_pipe"),
    msg = paste(
      "`con` must be a class of `vault_pipe`",
      "Use `open_vault()` to create a connection"
    )
  )

  assertthat::assert_that(
    all(names(con) %in% c("data", "db_con")),
    msg = paste(
      "con must have `data` and `db_con`",
      "Use `open_vault()` to create a connection"
    )
  )

  sel_data <- con$data

  assertthat::assert_that(
    inherits(sel_data, "tbl"),
    msg = "data must be a class of `tbl`"
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "path does not lead to valid SQLite database"
  )

  assertthat::assert_that(
    is.logical(return_raw_data),
    msg = "The 'return_raw_data' must be a logical"
  )

  assertthat::assert_that(
    is.logical(check_mandatory_references),
    msg = "The 'check_mandatory_references' must be a logical"
  )

  assertthat::assert_that(
    is.logical(verbose),
    msg = "The 'verbose' must be a logical"
  )

  nrow_mandatory_references <- 0

  if (
    isTRUE(check_mandatory_references)
  ) {
    data_mandatory_references <-
      get_references(
        con = con,
        get_source = FALSE,
        verbose = verbose
      )

    if (
      is.null(data_mandatory_references) %>%
        isFALSE() %>%
        isTRUE()
    ) {
      nrow_mandatory_references <-
        data_mandatory_references %>%
        dplyr::filter(
          .data$mandatory == TRUE
        ) %>%
        nrow()
    }
  }

  if (
    nrow_mandatory_references > 0 &&
      isTRUE(verbose)
  ) {
    message(
      "!!! The data contains mandatory references !!!",
      "Please make sure to run `get_references()` before extracting data"
    )
  }

  if (
    isTRUE(return_raw_data)
  ) {
    res <-
      sel_data %>%
      dplyr::collect()

    return(res)
  }

  data_readble <-
    get_readable_column_names(
      con = sel_con,
      data = sel_data
    ) %>%
    dplyr::collect()

  data_packed <-
    pack_data(
      sel_data = data_readble,
      verbose = verbose
    )

  return(data_packed)
}
