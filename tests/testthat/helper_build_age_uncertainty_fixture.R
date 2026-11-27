build_age_uncertainty_fixture <- function(flag_primary_key = FALSE) {
  con_db <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = ":memory:"
    )

  DBI::dbExecute(
    conn = con_db,
    statement = stringr::str_c(
      "CREATE TABLE \"Samples\" ",
      "(\"sample_id\" INTEGER PRIMARY KEY, \"sample_name\" TEXT)"
    )
  )

  # The 2.0.0 layout keys the table by sample and iteration; the 1.0.0
  #   layout is a rowid table with a separate index on sample_id
  if (
    base::isTRUE(flag_primary_key)
  ) {
    vec_statement_uncertainty <-
      stringr::str_c(
        "CREATE TABLE \"SampleUncertainty\" ",
        "(\"sample_id\" INTEGER NOT NULL, \"iteration\" INTEGER NOT NULL, ",
        "\"age\" INTEGER, PRIMARY KEY (\"sample_id\", \"iteration\")) ",
        "WITHOUT ROWID"
      )
  } else {
    vec_statement_uncertainty <-
      c(
        stringr::str_c(
          "CREATE TABLE \"SampleUncertainty\" ",
          "(\"sample_id\" INTEGER, \"iteration\" INTEGER, \"age\" INTEGER)"
        ),
        stringr::str_c(
          "CREATE INDEX idx_sampleuncertainty_sample_id ",
          "ON SampleUncertainty(sample_id)"
        )
      )
  }

  purrr::walk(
    .x = vec_statement_uncertainty,
    .f = ~ DBI::dbExecute(
      conn = con_db,
      statement = .x
    )
  )

  DBI::dbWriteTable(
    conn = con_db,
    name = "Samples",
    value = tibble::tibble(
      sample_id = c(1L, 2L, 3L, 4L),
      sample_name = c("sample_1", "sample_2", "sample_3", "sample_4")
    ),
    append = TRUE
  )

  # Rows are stored out of order, so that only an ORDER BY sorts the result
  DBI::dbWriteTable(
    conn = con_db,
    name = "SampleUncertainty",
    value = tibble::tibble(
      sample_id = c(3L, 1L, 2L, 1L, 3L, 2L, 4L, 4L),
      iteration = c(2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L),
      age = c(320L, 120L, 210L, 110L, 310L, 220L, 420L, 410L)
    ),
    append = TRUE
  )

  # The selection holds samples 1 to 3; sample 4 is left out
  data_samples_selected <-
    dplyr::tbl(
      src = con_db,
      from = "Samples"
    ) |>
    dplyr::filter(
      .data[["sample_id"]] <= 3L
    )

  res <-
    base::structure(
      base::list(
        data = data_samples_selected,
        db_con = con_db
      ),
      class = c("list", "vault_pipe")
    )

  return(res)
}
