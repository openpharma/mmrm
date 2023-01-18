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
#' @return The matrix of covariance matrix of beta.
#'
#' @keywords internal
h_get_empirical <- function(tmb_data, theta, beta, beta_vcov, jackknife) {
  .Call(`_mmrm_get_empirical`, PACKAGE = "mmrm", tmb_data, theta, beta, beta_vcov, jackknife)
}
