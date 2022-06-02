#' Fitting `glmmTMB` Model
#'
#' This function helps to fit an glmmTMB model with a single optimizer, while capturing messages and warnings.
#'
#' @param formula (`formula`)\cr the `glmmTMB` formula.
#' @param data (`data`)\cr the data to be used for the model.
#' @param start (`list`)\cr a list of starting values.
#' @param optimizer (`character`)\cr optimizer to be used to generate the model.
#'
#' @return the fitted glmmTMB object, with additional attributes containing warnings,
#' messages, errors, optimizer used and convergence status.
#' @export
#'
#' @examples
#' \dontrun{
#' mod_fit <- fit_single_optimizer(
#' formula = h_build_formula(vs),
#' data = dat,
#' optimizer = "BFGS"
#' )
#' attr(mod_fit, "converged")
#' }
fit_single_optimizer <- function(formula,
                                 data,
                                 start = NULL,
                                 optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb")) {
  assert_formula(formula)
  assert_data_frame(data)
  assert_list(start, null.ok = TRUE)
  optimizer <- match.arg(optimizer)
  control <- glmmTMB::glmmTMBControl(
    optimizer = if (optimizer == "nlminb") stats::nlminb else stats::optim,
    optArgs = if (optimizer == "nlminb") list() else list(method = optimizer),
    parallel = 1L
  )
  quiet_fit <- h_record_all_output(
    glmmTMB::glmmTMB(
      formula = formula,
      data = data,
      dispformula = ~0,
      REML = TRUE,
      start = start,
      control = control
    ),
    remove = list(
      warnings = c(
        "OpenMP not supported.",
        "'giveCsparse' has been deprecated; setting 'repr = \"T\"' for you"
      )
    )
  )
  converged <- (length(quiet_fit$warnings) == 0L) &&
    (length(quiet_fit$errors) == 0L) &&
    (quiet_fit$result$fit$convergence == 0)
  structure(
    quiet_fit$result,
    errors = quiet_fit$errors,
    warnings = quiet_fit$warnings,
    messages = quiet_fit$messages,
    optimizer = optimizer,
    converged = converged,
    class = c("mmrm_fit", class(quiet_fit$result))
  )
}
