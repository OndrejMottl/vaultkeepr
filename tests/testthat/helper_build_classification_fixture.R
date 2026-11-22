build_classification_fixture <- function(
  vec_dataset_type = c("vegetation_plot", "fossil_pollen_archive")
) {
  # The box holds only part of the example database, so that a query
  #   narrowed to the selection differs from one over the whole database
  res <-
    open_vault(
      path = base::file.path(
        base::tempdir(),
        "example.sqlite"
      ),
      verbose = FALSE
    ) |>
    get_datasets() |>
    select_dataset_by_type(sel_dataset_type = vec_dataset_type) |>
    select_dataset_by_geo(
      long_lim = c(0, 20),
      lat_lim = c(40, 60),
      verbose = FALSE
    ) |>
    get_samples()

  return(res)
}
