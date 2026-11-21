# Small in-memory database for the box filter of select_dataset_by_geo().
# Eight datasets around the box of 0 to 90 degrees longitude and latitude:
#   1 vegetation_plot        inside (10, 10)
#   2 vegetation_plot        on the lower limits (0, 0)
#   3 fossil_pollen_archive  on the upper limits (90, 90)
#   4 traits                 outside in longitude (100, 10)
#   5 gridpoints             outside in latitude (10, -10)
#   6 vegetation_plot        missing longitude
#   7 traits                 missing latitude
#   8 traits                 inside (20, 20)
# Returns a vault_pipe after get_datasets(); the caller disconnects db_con.
build_geo_box_fixture <- function() {
  db_con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  DBI::dbWriteTable(
    conn = db_con,
    name = "DatasetTypeID",
    value = tibble::tibble(
      dataset_type_id = 1:4,
      dataset_type = c(
        "vegetation_plot",
        "fossil_pollen_archive",
        "traits",
        "gridpoints"
      )
    )
  )

  DBI::dbWriteTable(
    conn = db_con,
    name = "Datasets",
    value = tibble::tibble(
      dataset_id = 1:8,
      dataset_type_id = c(1L, 1L, 2L, 3L, 4L, 1L, 3L, 3L),
      coord_long = c(10, 0, 90, 100, 10, NA, 10, 20),
      coord_lat = c(10, 0, 90, 10, -10, 10, NA, 20)
    )
  )

  con_empty <-
    base::structure(
      .Data = base::list(
        data = tibble::tibble(),
        db_con = db_con
      ),
      class = c("list", "vault_pipe")
    )

  res <-
    get_datasets(con = con_empty)

  return(res)
}
