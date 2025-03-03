assertthat_cli <- function(exp, msg, verbose = TRUE) {
  if (
    isFALSE(exp)
  ) {
    fc_name <-
      sys.calls()[[sys.nframe() - 1]][1]

    error_msg <-
      paste0(
        "Error in function: ",
        "{.fn {fc_name}}", # name of caller
        ": ",
        msg
      )

    if (
      isTRUE(verbose)
    ) {
      cli::cli_alert_danger(error_msg)
    }


    stop_quietly()
  }
}
