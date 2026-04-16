#' @title Select samples by age
#' @description Select samples by age criteria
#' @param con A connection object created by `open_vault()`
#' @param age_lim A numeric vector of length 2.
#' The minimum and maximum age of the samples to be selected.
#' @param sel_dataset_type (optional) A character vector specifying the dataset types to filter
#' @param verbose A logical value specifying whether to print messages
#' @return A class of `vault_pipe`
#' @export
#' @details
#' There is an option to filter only specific dataset types (as some dataset types may not have age information).
#' To do this, use the `sel_dataset_type` argument.
select_samples_by_age <- function(
    con = NULL,
    age_lim = c(-Inf, Inf),
    sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive", "gridpoints"),
    verbose = TRUE) {
  .data <- rlang::.data

  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a {.cls vault_pipe} object. Use {.fn open_vault} to create a connection",
    verbose = verbose
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con} elements. Use {.fn open_vault} to create a connection",
    verbose = verbose
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code con$data} must be a {.cls tbl}",
    verbose = verbose
  )

  assertthat_cli(
    "age" %in% colnames(sel_data),
    msg = "The data does not contain the {.code age} column. Use {.fn get_samples} before this function",
    verbose = verbose
  )

  assertthat_cli(
    "dataset_type" %in% colnames(sel_data),
    msg = "The data does not contain the {.code dataset_type} column. Use {.fn select_dataset_by_type} before this function",
    verbose = verbose
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}",
    verbose = verbose
  )

  assertthat_cli(
    is.numeric(age_lim),
    msg = "{.arg age_lim} must be a numeric vector",
    verbose = verbose
  )

  assertthat_cli(
    length(age_lim) == 2,
    msg = "{.arg age_lim} must be a vector of length 2",
    verbose = verbose
  )

  age_lim_min <- as.numeric(eval(min(age_lim)))
  age_lim_max <- as.numeric(eval(max(age_lim)))

  assertthat_cli(
    is.character(sel_dataset_type),
    msg = "{.arg sel_dataset_type} must be a character vector",
    verbose = verbose
  )

  assertthat_cli(
    inherits(verbose, "logical"),
    msg = "{.arg verbose} must be a logical value",
    verbose = verbose
  )


  # Get common dataset type and print message if necessary
  sel_dataset_type <-
    get_common_dataset_type(
      data_source = sel_data,
      vec_dataset_type = sel_dataset_type,
      verbose = verbose
    )

  data_res <-
    sel_data %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        .default = TRUE,
        is.na(.data$age) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        (.data$age < age_lim_min) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        (.data$age > age_lim_max) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE
      )
    ) %>%
    dplyr::filter(
      .data$keep == TRUE
    ) %>%
    dplyr::select(-"keep")

  res <-
    structure(
      list(
        data = data_res,
        db_con = sel_con
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
