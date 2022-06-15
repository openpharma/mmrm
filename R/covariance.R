#' Getting Covariance Estimate and Number of Variance Parameters in Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param model (`mmrm_fit`)\cr model fit.
#'
#' @return The covariance matrix estimate, with
#'   additional attribute `n_parameters` (the number of variance parameters in
#'   the model).
#' @export
#'
#' @examples
#' mod_fit <- fit_single_optimizer(
#'   formula = FEV1 ~ ARMCD + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' h_cov_estimate(mod_fit)
h_cov_estimate <- function(model) {
  expect_class(model, "mmrm_fit")

  cov_est <- model$cov
  theta <- model$theta_est

  structure(
    cov_est,
    n_parameters = length(theta)
  )
}
