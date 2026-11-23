# Read the schema of a SQLite database: the columns of every table, with
#   type, default, NOT NULL and primary-key position, and the indexes of
#   every table, with their columns. Used to compare two databases.
load_schema_description <- function(db_con) {
  vec_tables <-
    DBI::dbGetQuery(
      conn = db_con,
      statement = stringr::str_c(
        "SELECT name FROM sqlite_master ",
        "WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ",
        "ORDER BY name"
      )
    ) |>
    dplyr::pull("name")

  data_columns <-
    purrr::map(
      .x = vec_tables,
      .f = ~ DBI::dbGetQuery(
        conn = db_con,
        statement = stringr::str_glue("PRAGMA table_info(\"{.x}\")")
      ) |>
        dplyr::mutate(table_name = .x)
    ) |>
    dplyr::bind_rows() |>
    dplyr::select(
      "table_name",
      "cid",
      column_name = "name",
      "type",
      "notnull",
      "dflt_value",
      "pk"
    ) |>
    tibble::as_tibble()

  data_indexes <-
    purrr::map(
      .x = vec_tables,
      .f = ~ DBI::dbGetQuery(
        conn = db_con,
        statement = stringr::str_glue("PRAGMA index_list(\"{.x}\")")
      ) |>
        dplyr::mutate(table_name = .x)
    ) |>
    dplyr::bind_rows() |>
    dplyr::select(
      "table_name",
      index_name = "name",
      "unique",
      "origin",
      "partial"
    ) |>
    dplyr::mutate(
      index_columns = purrr::map_chr(
        .x = .data[["index_name"]],
        .f = ~ DBI::dbGetQuery(
          conn = db_con,
          statement = stringr::str_glue("PRAGMA index_info(\"{.x}\")")
        ) |>
          dplyr::pull("name") |>
          stringr::str_c(collapse = ",")
      )
    ) |>
    dplyr::arrange(
      .data[["table_name"]],
      .data[["index_name"]]
    ) |>
    tibble::as_tibble()

  res <-
    list(
      columns = data_columns,
      indexes = data_indexes
    )

  return(res)
}
