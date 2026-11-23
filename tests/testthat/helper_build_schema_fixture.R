# Create the tables and indexes of a VegVault schema file in a database,
#   statement by statement. The file holds only CREATE statements, each
#   ending with a semicolon, and comments on lines that start with "--".
#   The file is read from the source tree when the tests run there
#   (devtools::test()), and from the installed package otherwise
#   (R CMD check), because base::system.file() sees only installed packages.
build_schema_fixture <- function(
  db_con,
  file_sql = "vegvault_schema_1.0.0.sql"
) {
  path_sql_source <-
    testthat::test_path("..", "..", "inst", "sql", file_sql)

  path_sql <-
    base::ifelse(
      test = base::file.exists(path_sql_source),
      yes = path_sql_source,
      no = base::system.file("sql", file_sql, package = "vaultkeepr")
    )

  if (
    !base::file.exists(path_sql)
  ) {
    cli::cli_abort("The schema file {.file {path_sql}} does not exist.")
  }

  vec_sql_lines <-
    base::readLines(
      con = path_sql,
      warn = FALSE
    )

  vec_sql_code <-
    vec_sql_lines[!stringr::str_detect(vec_sql_lines, pattern = "^\\s*--")]

  vec_statements <-
    stringr::str_c(vec_sql_code, collapse = "\n") |>
    stringr::str_split_1(pattern = ";") |>
    stringr::str_trim()

  vec_statements_nonempty <-
    vec_statements[base::nzchar(vec_statements)]

  purrr::walk(
    .x = vec_statements_nonempty,
    .f = ~ DBI::dbExecute(
      conn = db_con,
      statement = .x
    )
  )

  res <-
    base::length(vec_statements_nonempty)

  return(base::invisible(res))
}
