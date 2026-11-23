# Write the links between samples and nearby gridpoint samples in the
#   VegVault 2.0.0 layout of the test or vignette database. VegVault 2.0.0
#   stores a link once per position (longitude, latitude, age): `Positions`,
#   `SamplePosition` and `PositionLink`. The view `AbioticDataReference`
#   returns them once per sample, with the columns of VegVault 1.0.0.
#   `data_sample_links` holds the links per sample (`sample_id`,
#   `sample_ref_id`, `distance_in_km`, `distance_in_years`); positions are
#   written for every sample of the datasets of `vec_dataset_type_id`.
#   The same file is in tests/testthat/ and vignettes/;
#   test-helper_make_database.R checks that the two copies match.
build_position_link_fixture <- function(
  con_db,
  data_sample_links,
  vec_dataset_type_id = c(1L, 2L)
) {
  data_sample_coords <-
    dplyr::tbl(
      src = con_db,
      from = "Datasets"
    ) |>
    dplyr::filter(.data[["dataset_type_id"]] %in% vec_dataset_type_id) |>
    dplyr::select("dataset_id", "coord_long", "coord_lat") |>
    dplyr::inner_join(
      dplyr::tbl(
        src = con_db,
        from = "DatasetSample"
      ),
      by = "dataset_id"
    ) |>
    dplyr::inner_join(
      dplyr::tbl(
        src = con_db,
        from = "Samples"
      ) |>
        dplyr::select("sample_id", "age"),
      by = "sample_id"
    ) |>
    dplyr::distinct(
      .data[["sample_id"]],
      .data[["coord_long"]],
      .data[["coord_lat"]],
      .data[["age"]]
    ) |>
    dplyr::collect()

  data_positions <-
    data_sample_coords |>
    dplyr::distinct(
      .data[["coord_long"]],
      .data[["coord_lat"]],
      .data[["age"]]
    ) |>
    dplyr::arrange(
      .data[["coord_long"]],
      .data[["coord_lat"]],
      .data[["age"]]
    ) |>
    dplyr::mutate(position_id = dplyr::row_number())

  data_sample_positions <-
    data_sample_coords |>
    dplyr::inner_join(
      data_positions,
      by = c("coord_long", "coord_lat", "age")
    ) |>
    dplyr::select("sample_id", "position_id")

  data_position_links <-
    data_sample_links |>
    dplyr::inner_join(
      data_sample_positions,
      by = "sample_id"
    ) |>
    dplyr::distinct(
      .data[["position_id"]],
      .data[["sample_ref_id"]],
      .data[["distance_in_km"]],
      .data[["distance_in_years"]]
    )

  dplyr::copy_to(
    dest = con_db,
    df = dplyr::select(
      data_positions,
      "position_id", "coord_long", "coord_lat", "age"
    ),
    name = "Positions",
    append = TRUE
  )

  dplyr::copy_to(
    dest = con_db,
    df = data_sample_positions,
    name = "SamplePosition",
    append = TRUE
  )

  dplyr::copy_to(
    dest = con_db,
    df = data_position_links,
    name = "PositionLink",
    append = TRUE
  )

  res <-
    base::nrow(data_position_links)

  return(base::invisible(res))
}
