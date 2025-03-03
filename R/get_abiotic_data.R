#' @title Get abiotic data
#' @description
#' Get abiotic data from the Vault database. The function will filter out the
#' gridpoints samples in such way, to keep only one gridpoint sample per
#' non-gridpoint sample.
#' First it will filter out gridpoint samples to only keep samples that are
#' within the `limit_by_distance_km` and `limit_by_age_years` from non-gridpoint
#' Next, it add `sample_id_link` column to the non-gridpoint samples, which
#' is the closest gridpoint sample to the non-gridpoint sample.
#' Finally, it will add abiotic data to the gridpoint samples, specified by
#' `mode` argument. For `mode = "nearest"`, it will add the abiotic data from
#' the closest gridpoint sample. For `mode = "mean"` and `mode = "median"`, it
#' will first calculate the mean or median abiotic data for each
#' gridpoint sample.
#' The non-gridpoint samples are the samples that are not associated with the
#' @param con A connection object created by `open_vault()`
#' @param mode A character string specifying the mode of abiotic data
#'  aggregation. It must be one of `nearest`, `mean`, or `median`.
#' @param limit_by_distance_km A numeric value specifying the maximum distance
#' in kilometers between the non-gridpoint and gridpoint samples.
#' @param limit_by_age_years A numeric value specifying the maximum age in
#' years between the non-gridpoint and gridpoint samples.
#' @param verbose A logical value specifying whether to print messages
#' @return A connection object with abiotic data
#' @export
get_abiotic_data <- function(
    con = NULL,
    mode = c("nearest", "mean", "median"),
    limit_by_distance_km = 50,
    limit_by_age_years = 5e3,
    verbose = TRUE) {
  .data <- rlang::.data
  median <- stats::median
  `%>%` <- magrittr::`%>%`

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
    "dataset_type_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `dataset_type_id` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `sample_id` columns. Please add",
      "`get_samples()` to the pipe before this function."
    )
  )

  n_datasets_gridpoints <-
    sel_data %>%
    dplyr::filter(.data$dataset_type_id == 4) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  assertthat::assert_that(
    n_datasets_gridpoints > 0,
    msg = paste(
      "The dataset does not contain any gridpoints.",
      "Please make sure to not filter them out."
    )
  )

  n_datasets_non_gridpoints <-
    sel_data %>%
    dplyr::filter(.data$dataset_type_id != 4) %>%
    dplyr::count(name = "N") %>%
    dplyr::collect() %>%
    dplyr::pull("N")

  assertthat::assert_that(
    n_datasets_non_gridpoints > 0,
    msg = paste(
      "The dataset does not contain any non-gridpoints.",
      "Please make sure to not filter them out."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    "AbioticData" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "AbioticData table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "AbioticDataReference" %in% DBI::dbListTables(sel_con),
    msg = paste(
      "AbioticDataReference table does not exist in the Vault database",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    "sample_id" %in% colnames(dplyr::tbl(sel_con, "AbioticData")),
    msg = paste(
      "The AbioticData does not contain `sample_id` column in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  assertthat::assert_that(
    all(
      c(
        "sample_id",
        "sample_ref_id",
        "distance_in_km",
        "distance_in_years"
      ) %in%
        colnames(dplyr::tbl(sel_con, "AbioticDataReference"))
    ),
    msg = paste(
      "The AbioticDataReference does not contain columns",
      "`sample_id`, `sample_ref_id`, `distance_in_km`, `distance_in_years`",
      "in the Vault database.",
      "Make sure to connect to the correct database"
    )
  )

  mode <- match.arg(mode)

  assertthat::assert_that(
    mode %in% c("nearest", "mean", "median"),
    msg = paste(
      "`mode` must be one of `nearest`, `mean`, `median`"
    )
  )

  assertthat::assert_that(
    is.numeric(limit_by_distance_km),
    msg = paste(
      "`limit_by_distance_km` must be a numeric value"
    )
  )

  assertthat::assert_that(
    length(limit_by_distance_km) == 1,
    msg = paste(
      "`limit_by_distance_km` must be a single value"
    )
  )

  assertthat::assert_that(
    limit_by_distance_km > 0,
    msg = paste(
      "`limit_by_distance_km` must be greater than 0"
    )
  )

  assertthat::assert_that(
    is.numeric(limit_by_age_years),
    msg = paste(
      "`limit_by_age_years` must be a numeric value"
    )
  )

  assertthat::assert_that(
    length(limit_by_age_years) == 1,
    msg = paste(
      "`limit_by_age_years` must be a single value"
    )
  )

  assertthat::assert_that(
    limit_by_age_years > 0,
    msg = paste(
      "`limit_by_age_years` must be greater than 0"
    )
  )

  assertthat::assert_that(
    is.logical(verbose),
    msg = paste(
      "`verbose` must be a logical value"
    )
  )

  vec_vegetation_sample_id <-
    sel_data %>%
    dplyr::filter(.data$dataset_type_id != 4) %>%
    dplyr::distinct(.data$sample_id) %>%
    dplyr::collect() %>%
    dplyr::pull("sample_id")

  vec_gridpoints_sample_id <-
    sel_data %>%
    dplyr::filter(.data$dataset_type_id == 4) %>%
    dplyr::distinct(.data$sample_id) %>%
    dplyr::collect() %>%
    dplyr::pull("sample_id")


  if (
    isTRUE(verbose)
  ) {
    message(
      paste(
        "Getting the information about sample connections.",
        "This may take a while."
      )
    )
  }

  data_ref_subset <-
    dplyr::tbl(sel_con, "AbioticDataReference") %>%
    dplyr::filter(.data$sample_id %in% vec_vegetation_sample_id) %>%
    dplyr::filter(.data$sample_ref_id %in% vec_gridpoints_sample_id) %>%
    dplyr::filter(.data$distance_in_km <= limit_by_distance_km) %>%
    dplyr::filter(.data$distance_in_years <= limit_by_age_years)

  data_link_sub <-
    data_ref_subset %>%
    dplyr::group_by(.data$sample_id) %>%
    dplyr::filter(
      .data$distance_in_km == min(
        .data$distance_in_km,
        na.rm = TRUE
      )
    ) %>%
    dplyr::filter(
      .data$distance_in_years == min(
        .data$distance_in_years,
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "sample_id",
      "sample_ref_id"
    ) %>%
    # there are someties multiple gridpoints are closest to
    #   the same vegetation sample.
    #   In this case, we take the first one
    dplyr::distinct(.data$sample_id, .keep_all = TRUE)

  vec_gridpoints_sample_id_subset <-
    data_link_sub %>%
    dplyr::distinct(.data$sample_ref_id) %>%
    dplyr::collect() %>%
    dplyr::pull("sample_ref_id")

  sel_data_sub <-
    sel_data %>%
    dplyr::filter(
      .data$sample_id %in% c(
        vec_vegetation_sample_id,
        vec_gridpoints_sample_id_subset
      )
    ) %>%
    # join the gridpoints data and add reference to the closest vegetation point
    dplyr::left_join(
      data_link_sub,
      by = c("sample_id" = "sample_ref_id"),
      suffix = c("", "_link")
    )

  if (
    isTRUE(verbose)
  ) {
    message(
      switch(mode,
        "nearest" = "Getting value of the nearest gridpont data",
        "mean" = "Getting the mean value of all gridpoint within the limit",
        "median" = "Getting the median  value of all gridpoint within the limit"
      )
    )
  }

  if (
    mode == "nearest"
  ) {
    data_abiotic <-
      sel_data_sub %>%
      dplyr::left_join(
        dplyr::tbl(sel_con, "AbioticData"),
        by = c("sample_id")
      )
  }

  if (
    mode == "mean" ||
      mode == "median"
  ) {
    data_abiotic_grouped <-
      data_ref_subset %>%
      dplyr::left_join(
        dplyr::tbl(sel_con, "AbioticData"),
        by = c("sample_ref_id" = "sample_id")
      ) %>%
      dplyr::group_by(.data$sample_id, .data$abiotic_variable_id)

    # there is some issue to call `.data$abiotic_value` in the `switch`
    #   statement. So, we use `if` statement instead.
    if (
      mode == "mean"
    ) {
      data_abiotic_summed <-
        data_abiotic_grouped %>%
        dplyr::summarise(
          .groups = "drop",
          abiotic_value = mean(.data$abiotic_value, na.rm = TRUE)
        )
    } else {
      data_abiotic_summed <-
        data_abiotic_grouped %>%
        dplyr::summarise(
          .groups = "drop",
          abiotic_value = median(.data$abiotic_value, na.rm = TRUE)
        )
    }

    data_abiotic <-
      sel_data_sub %>%
      dplyr::left_join(
        data_abiotic_summed,
        by = c("sample_id_link" = "sample_id")
      )
  }

  res <-
    structure(
      list(
        data = data_abiotic,
        db_con = sel_con
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
