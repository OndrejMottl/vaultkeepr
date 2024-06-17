#' @title Only select taxa by ID
#' @description Filter the Samples in dataset by taxon id
#' @param con A connection to the Vault database
#' @param sel_id A integer vector of taxa ids
#' @return A vault_pipe object
#' @export
select_taxa_by_id <- function(con = NULL, sel_id = NULL) {
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
    "taxon_id" %in% colnames(sel_data),
    msg = paste(
      "The dataset does not contain `taxon_id` columns. Please add",
      "`get_taxa()` to the pipe before this function."
    )
  )

  sel_con <- con$db_con

  assertthat::assert_that(
    inherits(sel_con, "SQLiteConnection"),
    msg = "db_con must be a class of `SQLiteConnection`"
  )

  assertthat::assert_that(
    is.numeric(sel_id),
    msg = "`sel_id` must be a numeric vector"
  )

  assertthat::assert_that(
    all(round(sel_id) == sel_id),
    msg = "`sel_id` must be a integer vector"
  )

  assertthat::assert_that(
    length(sel_id) > 0,
    msg = "`sel_id` must have at least one taxon id"
  )

  data_res <-
    sel_data %>%
    dplyr::filter(
      .data$taxon_id %in% sel_id
    )

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
