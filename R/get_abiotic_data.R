#' @title Get abiotic data
#' @description Get abiotic data from the Vault database
#' @param con A connection object created by `open_vault()`
#' @return A connection object with abiotic data
#' @export
get_abiotic_data <- function(
    con = NULL,
    mode = c("nearest", "mean", "median"),
    limit_by_distance_km = 50,
    limit_by_age_years = 5e3,
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
    dplyr::distinct(sample_id, .keep_all = TRUE)

  vec_gridpoints_sample_id_subset <-
    data_link_sub %>%
    dplyr::distinct(.data$sample_ref_id) %>%
    dplyr::collect() %>%
    dplyr::pull("sample_ref_id")

  sel_data_sub <-
    sel_data %>%
    dplyr::filter(
      sample_id %in% c(
        vec_vegetation_sample_id,
        vec_gridpoints_sample_id_subset
      )
    ) %>%
    dplyr::left_join(
      data_link_sub,
      by = c("sample_id" = "sample_ref_id"),
      suffix = c("", "_link")
    )

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
    data_abiotic_summed <-
      data_ref_subset %>%
      dplyr::left_join(
        dplyr::tbl(sel_con, "AbioticData"),
        by = c("sample_ref_id" = "sample_id")
      ) %>%
      dplyr::group_by(.data$sample_id, .data$abiotic_variable_id) %>%
      dplyr::summarise(
        .groups = "drop",
        abiotic_value = switch(mode,
          "mean" = mean(.data$abiotic_value, na.rm = TRUE),
          "median" = median(.data$abiotic_value, na.rm = TRUE)
        )
      )

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
