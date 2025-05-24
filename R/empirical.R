#' Obtain Empirical/Jackknife/Bias-Reduced Covariance
#'
#' @description Obtain the empirical or Jackknife covariance for \eqn{\beta}.
#' Used in `mmrm` fitting if method is "Empirical", "Empirical-Jackknife" or
#' "Empirical-Bias-Reduced".
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta (`numeric`)\cr theta estimate.
#' @param beta (`numeric`)\cr beta estimate.
#' @param beta_vcov (`matrix`)\cr covariance of beta estimate.
#' @param type (`string`)\cr type of empirical method, including "Empirical", "Empirical-Jackknife"
#' and "Empirical-Bias-Reduced".
#'
#' @return Named list with elements:
#' - `cov`: `matrix` empirical covariance.
#' - `g_mat`: `matrix` to calculate Satterthwaite degrees of freedom.
#'
#' @note
#' This function used to return `df_mat`, which was equivalent to `crossproduct(g_mat)`. However,
#' executing the cross product in C++ was a costly matrix multiplication, in particular when the number of coefficients
#' and/or the number of subjects was large. Therefore this is now avoided and `g_mat` is returned instead.
#'
#' @keywords internal
h_get_empirical <- function(tmb_data, theta, beta, beta_vcov, type) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_numeric(theta)
  n_beta <- ncol(tmb_data$x_matrix)
  assert_numeric(beta, finite = TRUE, any.missing = FALSE, len = n_beta)
  assert_matrix(
    beta_vcov,
    mode = "numeric",
    any.missing = FALSE,
    nrows = n_beta,
    ncols = n_beta
  )
  assert_subset(
    type,
    c("Empirical", "Empirical-Jackknife", "Empirical-Bias-Reduced")
  )
  .Call(
    `_mmrm_get_empirical`,
    PACKAGE = "mmrm",
    tmb_data,
    theta,
    beta,
    beta_vcov,
    type
  )
}
