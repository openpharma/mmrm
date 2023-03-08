#' Obtain Empirical/Jackknife Covariance
#'
#' @description Obtain the empirical or Jackknife covariance for \eqn{\beta}.
#' Used in `mmrm` fitting if method is "Empirical" or "Empirical-Jackknife".
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta (`numeric`)\cr theta estimate.
#' @param beta (`numeric`)\cr beta estimate.
#' @param beta_vcov (`matrix`)\cr covariance of beta estimate.
#' @param jackknife (`flag`)\cr indicator of whether Jackknife covariance is returned.
#'
#' @return Named list with elements:
#' - `cov`: `matrix` empirical covariance.
#' - `df_mat`: `matrix` to calculate Satterthwaite degree of freedom.
#'
#' @keywords internal
h_get_empirical <- function(tmb_data, theta, beta, beta_vcov, type) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_numeric(theta)
  n_beta <- ncol(tmb_data$x_matrix)
  assert_numeric(beta, finite = TRUE, any.missing = FALSE, len = n_beta)
  assert_matrix(beta_vcov, mode = "numeric", any.missing = FALSE, nrows = n_beta, ncols = n_beta)
  assert_flag(type)
  .Call(`_mmrm_get_empirical`, PACKAGE = "mmrm", tmb_data, theta, beta, beta_vcov, type)
}
