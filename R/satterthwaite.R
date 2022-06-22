#' Covariance Matrix for Coefficients Given Variance Parameters
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param model (`mmrm_tmb`)\cr initial model fit.
#'
#' @return Function with argument `theta` that calculates the covariance matrix
#'   for the coefficient vector `beta`.
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
#' model <- mmrm_tmb(formula, fev_data)
#' fun <- h_covbeta_fun(model)
#' fun(model$theta_est)
#' model$beta_vcov
h_covbeta_fun <- function(model) {
  assert_class(model, "mmrm_tmb")

  function(theta) {
    reported <- model$tmb_object$report(theta)
    reported$beta_vcov
  }
}
