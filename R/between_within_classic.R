#' Calculation of between-within Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Calculates the estimate, standard error, degrees of freedom,
#' t statistic and p-value for one-dimensional contrast. Used in [df_1d()] if method is
#' "Between-within".
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @keywords internal
h_df_1d_bwc <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  est <- sum(contrast * component(object, "beta_est"))
  var <- h_quad_form_vec(contrast, component(object, "beta_vcov"))
  se <- sqrt(var)

  n_subjects <- component(object, "n_subjects")
  n_obs  <- component(object, "n_obs")

  bw_pars <- apply(X = object$tmb_data$x_matrix, MARGIN = 2, FUN = function(x) {
    n_unique <- nrow(unique(cbind(x, as.numeric(object$tmb_data$full_frame[[object$formula_parts$subject_var]]))))
    if (n_unique > n_subjects) "within"
    else "between"
  })
browser()
  n_pars_between <- sum(bw_pars == "between")
  n_pars_within  <- sum(bw_pars == "within")

  ddf_between <- n_subjects - n_pars_between
  ddf_within <- n_obs - n_subjects - n_pars_within

  df <- if (bw_pars[as.logical(contrast)] == "within") ddf_within else ddf_between
  df <- unname(df)

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

#' Calculation of between-within Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Calculates the between-within degrees of freedom, F statistic and p value for multi-dimensional contrast.
#' Used in [df_md()] if method is "Between-within".
#'
#' @param object (`mmrm`)\cr object.
#' @param contrast (`matrix`)\cr contrast matrix.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_df_md_bwc <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_matrix(contrast, mode = "numeric", any.missing = FALSE, ncols = length(component(object, "beta_est")))
  prec_contrast <- solve(h_quad_form_mat(contrast, component(object, "beta_vcov")))
  contrast_est <- component(object, "beta_est") %*% t(contrast)
  f_statistic <- as.numeric(1 / nrow(contrast) * h_quad_form_mat(contrast_est, prec_contrast))

  n_subjects <- component(object, "n_subjects")
  n_obs  <- component(object, "n_obs")

  bw_pars <- apply(X = object$tmb_data$x_matrix, MARGIN = 2, FUN = function(x) {
    n_unique <- nrow(unique(cbind(x, as.numeric(object$tmb_data$full_frame[[object$formula_parts$subject_var]]))))
    if (n_unique > n_subjects) "within"
    else "between"
  })

  n_pars_between <- sum(bw_pars == "between")
  n_pars_within  <- sum(bw_pars == "within")

  ddf_between <- n_subjects - n_pars_between
  ddf_within <- n_obs - n_subjects - n_pars_within

  browser()

  df <- apply(X = object$tmb_data$x_matrix[, as.logical(colSums(contrast)), drop = FALSE], MARGIN = 2, FUN = function(x) {
    n_unique <- nrow(unique(cbind(x, as.numeric(object$tmb_data$full_frame[[object$formula_parts$subject_var]]))))
    if (n_unique > n_subjects) ddf_within
    else ddf_between
  })
  df <- unname(min(df))

  list(
    num_df = nrow(contrast),
    denom_df = df,
    f_stat = f_statistic,
    p_val = stats::pf(f_statistic, nrow(contrast), df, lower.tail = FALSE)
  )
}
