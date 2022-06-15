#' Fitting an MMRM with Single Optimizer
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function helps to fit an MMRM using `TMB` with a single optimizer,
#' while capturing messages and warnings.
#'
#' @param formula (`formula`)\cr the model formula, see [mmrm_tmb()] for details.
#' @param data (`data`)\cr the data to be used for the model.
#' @param reml (`flag`)\cr whether restricted maximum likelihood (REML) estimation is used,
#'   otherwise maximum likelihood (ML) is used.
#' @param start (`numeric` or `NULL`)\cr optional starting values for the variance parameters.
#' @param optimizer (`character`)\cr optimizer to be used to generate the model.
#'
#' @return The `mmrm_fit` object, with additional attributes containing warnings,
#'   messages, errors, optimizer used and convergence status in addition to the
#'   `mmrm_tmb` contents.
#' @export
#'
#' @examples
#' mod_fit <- fit_single_optimizer(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data,
#'   optimizer = "nlminb"
#' )
#' attr(mod_fit, "converged")
fit_single_optimizer <- function(formula,
                                 data,
                                 reml = TRUE,
                                 start = NULL,
                                 optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb")) {
  assert_formula(formula)
  assert_data_frame(data)
  assert_numeric(start, null.ok = TRUE)
  optimizer <- match.arg(optimizer)
  control <- h_mmrm_tmb_control(
    optimizer = if (optimizer == "nlminb") stats::nlminb else stats::optim,
    optimizer_control = if (optimizer == "nlminb") list(iter.max = 300, eval.max = 400) else list(),
    optimizer_args = if (optimizer == "nlminb") list() else list(method = optimizer)
  )
  quiet_fit <- h_record_all_output(
    mmrm_tmb(
      formula = formula,
      data = data,
      reml = reml,
      start = start,
      control = control
    )
  )
  converged <- (length(quiet_fit$warnings) == 0L) &&
    (length(quiet_fit$errors) == 0L) &&
    (quiet_fit$result$opt_details$convergence == 0)
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
