#' @title Select abiotic data by distance
#' @description Filter the abitotic data by distance from the vegetation data
#' @param con A class of `vault_pipe`
#' @param sel_km_distance A maximal distance in km from the vegetation data
#' @param sel_degree_grid
#' A a helper grid size in degrees to help to minime the caluclation proces
#' @param sel_dataset_type
#' A dataset types to use to as basis for the distance calculation
#' @param verbose A logical value specifying whether to print messages
#' @return A class of `vault_pipe`
#' @export
#' @details
#' The function filters the data by distance from the vegetation data.
#' It will first subset gripoints (abitoic data) that are within the same
#' square calculated by `sel_degree_grid`.
#' It will then estimate the excat distance from the vegetation data and
#' filter out all points abothe the `sel_km_distance` threshold.
#' Warning: This function may take a while to run, especially for big datasets.
#' `r lifecycle::badge("experimental")`
select_abiotic_data_by_distance <- function(
    con = NULL,
    sel_km_distance = NULL,
    sel_degree_grid = 0.5,
    sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive"),
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
    all(c("coord_long", "coord_lat") %in% colnames(sel_data)),
    msg = paste(
      "The data does not contain lat/long columns function.",
      "Use `get_datasets()` to the pipe before this function."
    )
  )

  assertthat::assert_that(
    "dataset_type" %in% colnames(sel_data),
    msg = paste(
      "The data does not contain `dataset_type` columns. Please add",
      "`get_datasets()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )


  assertthat::assert_that(
    is.numeric(sel_km_distance),
    msg = "sel_km_distance must be a numeric vector"
  )

  assertthat::assert_that(
    length(sel_km_distance) == 1,
    msg = "sel_km_distance must be a vector of length 1"
  )

  assertthat::assert_that(
    sel_km_distance > 0,
    msg = "sel_km_distance must be a a positive number"
  )

  assertthat::assert_that(
    is.numeric(sel_degree_grid),
    msg = "sel_degree_grid must be a numeric vector"
  )

  assertthat::assert_that(
    length(sel_degree_grid) == 1,
    msg = "sel_degree_grid must be a vector of length 1"
  )

  assertthat::assert_that(
    sel_degree_grid > 0,
    msg = "sel_degree_grid must be a a positive number"
  )

  assertthat::assert_that(
    inherits(verbose, "logical"),
    msg = "`verbose` must be a logical value"
  )

  sel_dataset_type <-
    get_common_dataset_type(
      data_source = sel_data,
      vec_dataset_type = sel_dataset_type,
      verbose = verbose
    )

  if (
    isTRUE(verbose)
  ) {
    message((
      paste(
        "Creating a bounding box and calculating distance.",
        "This may take a while..."
      )
    ))
  }

  data_vegetation_bounding_box <-
    sel_data %>%
    dplyr::filter(
      .data$dataset_type %in% sel_dataset_type
    ) %>%
    dplyr::distinct(
      .data$coord_long,
      .data$coord_lat
    ) %>%
    dplyr::mutate(
      coord_long_bin = floor(
        .data$coord_long / sel_degree_grid
      ) * sel_degree_grid,
      coord_lat_bin = floor(
        .data$coord_lat / sel_degree_grid
      ) * sel_degree_grid
    )

  data_vegetation_presece_box <-
    data_vegetation_bounding_box %>%
    dplyr::distinct(.data$coord_long_bin, .data$coord_lat_bin) %>%
    dplyr::mutate(
      data_present = TRUE
    )

  data_gridpoints_presence_box <-
    sel_data %>%
    dplyr::filter(
      .data$dataset_type %in% "gridpoints"
    ) %>%
    dplyr::distinct(
      .data$dataset_id,
      .data$coord_long,
      .data$coord_lat
    ) %>%
    dplyr::mutate(
      coord_long_bin = floor(
        .data$coord_long / sel_degree_grid
      ) * sel_degree_grid,
      coord_lat_bin = floor(
        .data$coord_lat / sel_degree_grid
      ) * sel_degree_grid
    ) %>%
    dplyr::left_join(
      data_vegetation_presece_box,
      by = c("coord_long_bin", "coord_lat_bin")
    ) %>%
    dplyr::filter(
      .data$data_present == TRUE
    ) %>%
    dplyr::select(-"data_present")

  data_to_calcutale_distance <-
    dplyr::left_join(
      data_gridpoints_presence_box,
      data_vegetation_bounding_box,
      by = c("coord_long_bin", "coord_lat_bin"),
      suffix = c("_grid", "_veg")
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  data_gridpoints_valid_ids <-
    data_to_calcutale_distance %>%
    dplyr::mutate(
      distance_in_m = purrr::pmap_dbl(
        .progress = TRUE,
        .l = list(
          .data$coord_long_grid,
          .data$coord_lat_grid,
          .data$coord_long_veg,
          .data$coord_lat_veg
        ),
        .f = ~ geosphere::distGeo(
          c(..1, ..2),
          c(..3, ..4)
        )
      )
    ) %>%
    dplyr::mutate(keep = (.data$distance_in_m / 1e3) < sel_km_distance) %>%
    dplyr::select("dataset_id", "keep") %>%
    dplyr::group_by(.data$dataset_id) %>%
    dplyr::summarise(
      .groups = "drop",
      keep_any = any(.data$keep),
    ) %>%
    dplyr::filter(.data$keep_any) %>%
    dplyr::distinct(.data$dataset_id) %>%
    dplyr::pull(.data$dataset_id)

  data_res <-
    sel_data %>%
    dplyr::mutate(
      keep = dplyr::case_when(
        .default = TRUE,
        !(.data$dataset_id %in% data_gridpoints_valid_ids) &
          .data$dataset_type == "gridpoints" ~ FALSE
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
