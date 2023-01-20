#' Replace Errors's Call Before Raising
#'
#' To circumvent `checkmate::reportAssertions`' immediate raising of errors with
#' a top-level call, this function captures the errors and applies a call to the
#' condition before it is raised, providing more localized and informative error
#' messages.
#'
#' @param expr (`expression`)\cr
#'   An expression to evaluate in the parent frame, which may raise a
#'   `condition`.
#' @param call (`call`)\cr
#'   A call to associate with any errors before they are raised.
#'
#' @return The result of evaluating `expr` if no errors are raised. An error is
#'   raised is otherwise.
#'
#' @keywords internal
with_error_call <- function(expr, call = sys.call(-1)) {
  expr <- substitute(expr)
  res <- tryCatch(
    eval.parent(expr),
    error = function(e) {
      e$call <- call
      stop(e)
    }
  )

  res
}
