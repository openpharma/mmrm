#' Calculation of Residual Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Calculates the estimate, standard error, degrees of freedom,
#' t statistic and p-value for one-dimensional contrast. Used in [df_1d()] if method is
#' "Residual".
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @keywords internal
h_df_1d_res <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  est <- sum(contrast * component(object, "beta_est"))
  var <- h_quad_form_vec(contrast, component(object, "beta_vcov"))
  se <- sqrt(var)
  df <- component(object, "n_obs") - length(component(object, "beta_est"))
  t_stat <- est / se
  p_val <- 2 * stats::pt(q = abs(t_stat), df = df, lower.tail = FALSE)
  list(
    est = est,
    se = se,
    df = df,
    t_stat = t_stat,
    p_val = p_val
  )
}

#' Calculation of Residual Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Calculates the residual degrees of freedom, F statistic and p value for multi-dimensional contrast.
#' Used in [df_md()] if method is "Residual".
#'
#' @param object (`mmrm`)\cr object.
#' @param contrast (`matrix`)\cr contrast matrix.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_df_md_res <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_matrix(contrast, mode = "numeric", any.missing = FALSE, ncols = length(component(object, "beta_est")))
  prec_contrast <- solve(h_quad_form_mat(contrast, component(object, "beta_vcov")))
  contrast_est <- component(object, "beta_est") %*% t(contrast)
  f_statistic <- 1 / nrow(contrast) * h_quad_form_mat(contrast_est, prec_contrast)
  df <- component(object, "n_obs") - length(component(object, "beta_est"))
  list(
    num_df = nrow(contrast),
    denom_df = df,
    f_stat = f_statistic,
    p_val = stats::pf(f_statistic, nrow(contrast), df, lower.tail = FALSE)
  )
}
