#' @title Select dataset by geographical location
#' @description Filter the dataset by geographical location
#' @param con A class of `vault_pipe`
#' @param long_lim A numeric vector of length 2 specifying the longitude limits
#' @param lat_lim A numeric vector of length 2 specifying the latitude limits
#' @param sel_dataset_type (optional) A character vector specifying the dataset types to filter
#' @param verbose A logical value specifying whether to print messages
#' @return A class of `vault_pipe`
#' @export
#' @details
#' There is an option to filter only specific dataset types (as some dataset types may not have lat/long information).
#' To do this, use the `sel_dataset_type` argument.
select_dataset_by_geo <- function(
    con = NULL,
    long_lim = c(-180, 180),
    lat_lim = c(-90, 90),
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
    all(c("coord_long", "coord_lat") %in% colnames(sel_data)),
    msg = "The data does not contain {.code coord_long} and {.code coord_lat} columns. Use {.fn get_datasets} before this function",
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

  assertthat_cli(
    inherits(verbose, "logical"),
    msg = "{.arg verbose} must be a logical value",
    verbose = verbose
  )


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
        is.na(.data$coord_long) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        is.na(.data$coord_lat) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        (.data$coord_long < long_lim_min) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        (.data$coord_long > long_lim_max) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        (.data$coord_lat < lat_lim_min) &
          (.data$dataset_type %in% c(
            sel_dataset_type
          )) ~ FALSE,
        (.data$coord_lat > lat_lim_max) &
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
