#' Accumulate Check Results
#'
#' @param severity (`string`)\cr
#'   A severity name to provide, expecting `"warn"` or `"stop"`
#' @param cond (`bool`)\cr
#'   A condition upon which the message should be emitted
#' @param ...
#'   A message to emit
#' @param envir
#'   The environment in which a hidden `..checks..` object should be
#'   accumulated. Defaults to the parent frame.
#'
#' @return The condition used to decide whether to emit a message
#'
#' @keywords internal
check <- function(severity, cond, ..., envir = parent.frame()) {
  if (!exists("..checks..", envir = envir))
    envir$..checks.. <- character()

  if (!cond) {
    n <- length(envir$..checks..)
    envir$..checks..[n + 1] <- paste(...)
    names(envir$..checks..)[n + 1] <- severity
  }

  cond
}



#' @describeIn check
#' Format messages accumulated by `check()` for printing as warnings or errors.
#'
#' @param x (`character`)\cr
#'   A named character vector. A subset of this vector is formatted if its name
#'   matches the provided severity.
#'
#' @return A formatted `character` vector
#'
#' @keywords internal
format_check_messages <- function(x, severity) {
  x <- x[names(x) == severity]
  x <- lapply(x, strwrap, exdent = 3, width = getOption("width", 80) - 3)
  x <- lapply(x, paste0, collapse = "\n")
  paste0(" * ", x, collapse = "\n")
}



#' @describeIn check
#' Emit check messages accumulated by calls to `check()`
#'
#' @param envir (`environment`)\cr
#'   Where to look for the `check()` accumulator
#'
#' @return `TRUE` (invisibly) if success, otherwise errors are emitted
#'
#' @keywords internal
emit_check_results <- function(envir = parent.frame()) {
  checks <- envir$..checks..

  if ("warn" %in% names(checks))
    warning(simpleWarning(
      paste0("\n", format_check_messages(checks, "warn")),
      call = sys.call(-1)
    ))

  if ("stop" %in% names(checks))
    stop(simpleError(
      paste0("\n", format_check_messages(checks, "stop")),
      call = sys.call(-1)
    ))

  invisible(TRUE)
}
