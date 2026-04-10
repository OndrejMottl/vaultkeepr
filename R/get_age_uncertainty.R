#' @title Get age uncertainties for samples
#' @description
#' For each Sample, get age uncertainty estimates from the Vault database.
#' Age uncertainties represent multiple iterations of age-depth modelling
#' and are stored in the `SampleUncertainty` table.
#' @param con
#' A `vault_pipe` object created by `open_vault()`. Must already contain
#' `sample_id` in the data, i.e. `get_samples()` must have been called
#' earlier in the pipe.
#' @param return_raw_data
#' A `logical` indicating whether to return raw long-format data.
#' If `FALSE` (default), returns a wide `tibble` with one row per
#' sample identified by `sample_name` and one column per iteration
#' (e.g. `iteration_1`, `iteration_2`, ...).
#' If `TRUE`, returns the raw long-format `tibble` with columns
#' `sample_id`, `iteration`, and `age_uncertainty`.
#' @return
#' When `return_raw_data = FALSE` (default): a wide `tibble` with
#' `sample_name` as the key column and one column per age-model
#' iteration (e.g. `iteration_1`, `iteration_2`, ...), restricted to
#' the samples present in the incoming pipe.
#' When `return_raw_data = TRUE`: a long `tibble` with columns
#' `sample_id`, `iteration`, and `age_uncertainty`.
#' @seealso [open_vault()], [get_samples()]
#' @export
get_age_uncertainty <- function(con = NULL, return_raw_data = FALSE) {
  assertthat_cli(
    inherits(con, "vault_pipe"),
    msg = "{.arg con} must be a {.cls vault_pipe} object. Use {.fn open_vault} to create a connection"
  )

  assertthat_cli(
    all(names(con) %in% c("data", "db_con")),
    msg = "{.arg con} must have {.code data} and {.code db_con} elements. Use {.fn open_vault} to create a connection"
  )

  sel_data <- con$data

  assertthat_cli(
    inherits(sel_data, "tbl"),
    msg = "{.code con$data} must be a {.cls tbl}"
  )

  assertthat_cli(
    "sample_id" %in% colnames(sel_data),
    msg = "The data does not contain the {.code sample_id} column. Use {.fn get_samples} before this function"
  )

  sel_con <- con$db_con

  assertthat_cli(
    inherits(sel_con, "SQLiteConnection"),
    msg = "{.code con$db_con} must be a {.cls SQLiteConnection}"
  )

  assertthat_cli(
    is.logical(return_raw_data),
    msg = "{.arg return_raw_data} must be a logical"
  )

  assertthat_cli(
    "SampleUncertainty" %in% DBI::dbListTables(sel_con),
    msg = "The {.code SampleUncertainty} table does not exist in the database. Make sure to connect to the correct database"
  )

  assertthat_cli(
    "sample_id" %in% colnames(
      dplyr::tbl(sel_con, "SampleUncertainty")
    ),
    msg = "The {.code SampleUncertainty} table does not contain the {.code sample_id} column. Make sure to connect to the correct database"
  )

  # Return only the uncertainty columns for the relevant samples.
  # Rename `age` -> `age_uncertainty` to make the column's meaning
  # explicit when the caller combines this with other data.
  data_res_raw <-
    dplyr::tbl(sel_con, "SampleUncertainty") %>%
    dplyr::rename(age_uncertainty = "age") %>%
    dplyr::semi_join(
      sel_data,
      by = "sample_id"
    )

  if (
    isTRUE(return_raw_data)
  ) {
    res <-
      dplyr::collect(data_res_raw)

    return(res)
  }

  data_samples <-
    dplyr::tbl(sel_con, "Samples") %>%
    dplyr::select("sample_id", "sample_name")

  data_res_named <-
    data_res_raw %>%
    dplyr::left_join(
      data_samples,
      by = "sample_id"
    ) %>%
    dplyr::select("sample_name", "iteration", "age_uncertainty") %>%
    dplyr::collect()

  res <-
    tidyr::pivot_wider(
      data_res_named,
      names_from = "iteration",
      values_from = "age_uncertainty",
      names_prefix = "iteration_"
    )

  return(res)
}
