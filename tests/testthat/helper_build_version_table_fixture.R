build_version_table_fixture <- function(
  vec_version,
  sel_layout = c("2.0.0", "1.0.0"),
  .local_envir = base::parent.frame()
) {
  sel_layout_matched <-
    base::match.arg(sel_layout)

  path_fixture <-
    base::tempfile(fileext = ".sqlite")

  withr::defer(
    base::unlink(path_fixture),
    envir = .local_envir
  )

  con_fixture <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = path_fixture
    )

  withr::defer(
    DBI::dbDisconnect(conn = con_fixture)
  )

  # A table besides version_control, so that the file is a valid SQLite
  #   database also when it has no version table
  DBI::dbExecute(
    conn = con_fixture,
    statement = "CREATE TABLE Datasets (dataset_id INTEGER PRIMARY KEY)"
  )

  # vec_version = NULL leaves out the version table
  if (
    base::is.null(vec_version)
  ) {
    return(path_fixture)
  }

  # The 2.0.0 layout follows the VegVault 2.0.0 schema; the 1.0.0 layout
  #   follows the test database of the package
  list_sql_create <-
    base::switch(
      sel_layout_matched,
      "2.0.0" = list(
        stringr::str_c(
          "CREATE TABLE version_control (id INTEGER PRIMARY KEY, ",
          "version TEXT NOT NULL UNIQUE, ",
          "update_date TEXT DEFAULT CURRENT_DATE, changelog TEXT, ",
          "build_profile TEXT, git_commit TEXT, manifest_sha256 TEXT)"
        )
      ),
      "1.0.0" = list(
        stringr::str_c(
          "CREATE TABLE version_control (id INTEGER PRIMARY KEY, ",
          "version TEXT, ",
          "update_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, ",
          "changelog TEXT)"
        ),
        stringr::str_c(
          "CREATE UNIQUE INDEX idx_version_control_id ",
          "ON version_control(id)"
        )
      )
    )

  purrr::walk(
    .x = list_sql_create,
    .f = ~ DBI::dbExecute(
      conn = con_fixture,
      statement = .x
    )
  )

  # Rows are written in the order given, so the ids follow vec_version
  if (
    base::length(vec_version) > 0L
  ) {
    DBI::dbExecute(
      conn = con_fixture,
      statement = "INSERT INTO version_control (version) VALUES (?)",
      params = list(vec_version)
    )
  }

  return(path_fixture)
}
