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
  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a class of {.cls vault_pipe}. Use {.fn open_vault} to create a connection.",
    verbose = verbose
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con}. Use {.fn open_vault} to create a connection.",
    verbose = verbose
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code data} must be a class of {.cls tbl}.",
    verbose = verbose
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.arg con} does not contain a valid SQLite database connection.",
    verbose = verbose
  )

  assertthat_cli(
    is.logical(return_raw_data),
    msg = "{.arg return_raw_data} must be a logical value.",
    verbose = verbose
  )

  assertthat_cli(
    is.logical(check_mandatory_references),
    msg = "{.arg check_mandatory_references} must be a logical value.",
    verbose = verbose
  )

  assertthat_cli(
    is.logical(verbose),
    msg = "{.arg verbose} must be a logical value."
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
    cli::cli_alert_warning(
      "The data contains mandatory references. Please make sure to run {.fn get_references} before extracting data."
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
