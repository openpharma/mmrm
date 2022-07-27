#' Capture all Output
#'
#' This function silences all warnings, errors & messages and instead returns a list
#' containing the results (if it didn't error) + the warning and error messages as
#' character vectors.
#'
#' @param expr (`expression`)\cr to be executed.
#' @param remove (`list`)\cr optional list with elements `warnings`, `errors`,
#'   `messages` which can be character vectors, which will be removed from the
#'   results if specified.
#'
#' @return
#' A list containing
#'
#' - `result`: The object returned by `expr` or `list()` if an error was thrown.
#' - `warnings`: `NULL` or a character vector if warnings were thrown.
#' - `errors`: `NULL` or a string if an error was thrown.
#' - `messages`: `NULL` or a character vector if messages were produced.
#'
#' @keywords internal
h_record_all_output <- function(expr, remove = list()) {
  # Note: We don't need to and cannot assert `expr` here.
  assert_list(remove)
  env <- new.env()
  result <- withCallingHandlers(
    withRestarts(
      expr,
      muffleStop = function() list()
    ),
    message = function(m) {
      msg_without_newline <- gsub(m$message, pattern = "\n$", replacement = "")
      env$message <- c(env$message, msg_without_newline)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      env$warning <- c(env$warning, w$message)
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      env$error <- c(env$error, e$message)
      invokeRestart("muffleStop")
    }
  )
  list(
    result = result,
    warnings = setdiff(env$warning, remove$warnings),
    errors = setdiff(env$error, remove$errors),
    messages = setdiff(env$message, remove$messages)
  )
}
