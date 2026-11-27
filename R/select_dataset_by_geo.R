#' @title Select dataset by geographical location
#' @description
#' Keep only the datasets that lie inside a box of longitude and latitude
#' limits. The filter stays lazy: SQLite receives range conditions on
#' `coord_long` and `coord_lat`, which the coordinate index of `Datasets` can
#' serve.
#' @param con A class of `vault_pipe`
#' @param long_lim A numeric vector of length 2 specifying the longitude limits
#' @param lat_lim A numeric vector of length 2 specifying the latitude limits
#' @param sel_dataset_type
#' A character vector of the dataset types the box applies to. Default is all
#' four types (`vegetation_plot`,
#' `fossil_pollen_archive`, `traits`, `gridpoints`), so every dataset outside
#' the box is dropped. Datasets of a type left out pass the filter wherever
#' they lie.
#' @param verbose A logical value specifying whether to print messages
#' @return A class of `vault_pipe`
#' @export
#' @details
#' The limits are inclusive and may be given in either order. A dataset of a
#' type the box applies to is dropped when `coord_long` or `coord_lat` is
#' missing, also with the default limits.
#'
#' To keep all datasets of a type, for example trait datasets from anywhere,
#' leave the type out of `sel_dataset_type`. A message then names the types in
#' the data that the box does not filter.
select_dataset_by_geo <- function(
    con = NULL,
    long_lim = c(-180, 180),
    lat_lim = c(-90, 90),
    sel_dataset_type = c(
      "vegetation_plot",
      "fossil_pollen_archive",
      "traits",
      "gridpoints"
    ),
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
    all(c("coord_long", "coord_lat") %in% colnames(sel_data)),
    msg = "The data does not contain {.code coord_long} and {.code coord_lat} columns. Use {.fn get_datasets} before this function",
    verbose = verbose
  )

  assertthat_cli(
    "dataset_type" %in% colnames(sel_data),
    msg = "The data does not contain the {.code dataset_type} column. Use {.fn select_dataset_by_type} before this function",
    verbose = verbose
  )

  assertthat_cli(
    exp = "dataset_type_id" %in% colnames(sel_data),
    msg = stringr::str_c(
      "The data does not contain the {.code dataset_type_id} column. ",
      "Use {.fn get_datasets} before this function"
    ),
    verbose = verbose
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}",
    verbose = verbose
  )

  assertthat_cli(
    is.numeric(long_lim),
    msg = "{.arg long_lim} must be a numeric vector",
    verbose = verbose
  )

  assertthat_cli(
    is.numeric(lat_lim),
    msg = "{.arg lat_lim} must be a numeric vector",
    verbose = verbose
  )

  assertthat_cli(
    length(long_lim) == 2,
    msg = "{.arg long_lim} must be a vector of length 2",
    verbose = verbose
  )
  assertthat_cli(
    length(lat_lim) == 2,
    msg = "{.arg lat_lim} must be a vector of length 2",
    verbose = verbose
  )

  long_lim_min <- as.numeric(eval(min(long_lim)))
  long_lim_max <- as.numeric(eval(max(long_lim)))

  lat_lim_min <- as.numeric(eval(min(lat_lim)))
  lat_lim_max <- as.numeric(eval(max(lat_lim)))

  assertthat_cli(
    is.character(sel_dataset_type),
    msg = "{.arg sel_dataset_type} must be a character vector",
    verbose = verbose
  )

  vec_dataset_type_all <-
    c(
      "vegetation_plot",
      "fossil_pollen_archive",
      "traits",
      "gridpoints"
    )

  assertthat_cli(
    exp = all(sel_dataset_type %in% vec_dataset_type_all),
    msg = stringr::str_c(
      "{.arg sel_dataset_type} must be one of {.code vegetation_plot}, ",
      "{.code fossil_pollen_archive}, {.code traits}, or {.code gridpoints}"
    ),
    verbose = verbose
  )

  assertthat_cli(
    inherits(verbose, "logical"),
    msg = "{.arg verbose} must be a logical value",
    verbose = verbose
  )

  vec_dataset_type_pass <-
    setdiff(
      x = vec_dataset_type_all,
      y = sel_dataset_type
    )

  # The message naming the types that pass is the only effect of this call
  if (
    length(vec_dataset_type_pass) > 0 &&
      isTRUE(verbose)
  ) {
    get_common_dataset_type(
      data_source = sel_data,
      vec_dataset_type = sel_dataset_type,
      verbose = verbose
    )
  }

  # Literal ids (at most four, from the lookup table) let SQLite combine
  #   the coordinate index with the type index; a subquery makes it scan
  if (
    length(vec_dataset_type_pass) > 0
  ) {
    vec_dataset_type_id_pass <-
      dplyr::tbl(
        src = sel_con,
        from = "DatasetTypeID"
      ) |>
      dplyr::filter(
        .data[["dataset_type"]] %in% vec_dataset_type_pass
      ) |>
      dplyr::pull(var = "dataset_type_id")
  } else {
    vec_dataset_type_id_pass <-
      integer(0)
  }

  if (
    length(vec_dataset_type_id_pass) == 0
  ) {
    data_res <-
      sel_data |>
      dplyr::filter(
        .data[["coord_long"]] >= !!long_lim_min,
        .data[["coord_long"]] <= !!long_lim_max,
        .data[["coord_lat"]] >= !!lat_lim_min,
        .data[["coord_lat"]] <= !!lat_lim_max
      )
  } else {
    data_res <-
      sel_data |>
      dplyr::filter(
        (
          .data[["coord_long"]] >= !!long_lim_min &
            .data[["coord_long"]] <= !!long_lim_max &
            .data[["coord_lat"]] >= !!lat_lim_min &
            .data[["coord_lat"]] <= !!lat_lim_max
        ) |
          .data[["dataset_type_id"]] %in% !!vec_dataset_type_id_pass
      )
  }

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
