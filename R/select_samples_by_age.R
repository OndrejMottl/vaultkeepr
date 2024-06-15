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

  assertthat::assert_that(
    "age" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `age` columns. Please add",
      "`get_samples()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "dataset_type" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `dataset_type` columns. Please add",
      "`select_dataset_by_type()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    is.numeric(age_lim),
    msg = "age_lim must be a numeric vector"
  )

  assertthat::assert_that(
    length(age_lim) == 2,
    msg = "age_lim must be a vector of length 2"
  )

  age_lim_min <- as.numeric(eval(min(age_lim)))
  age_lim_max <- as.numeric(eval(max(age_lim)))

  assertthat::assert_that(
    is.character(sel_dataset_type),
    msg = "`sel_dataset_type` must be a character vector"
  )

  assertthat::assert_that(
    inherits(verbose, "logical"),
    msg = "`verbose` must be a logical value"
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
