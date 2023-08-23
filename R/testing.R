#' Calculation of Degrees of Freedom for One-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#' Calculates the estimate, adjusted standard error, degrees of freedom,
#' t statistic and p-value for one-dimensional contrast.
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @export
#'
#' @examples
#' object <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' contrast <- numeric(length(object$beta_est))
#' contrast[3] <- 1
#' df_1d(object, contrast)
df_1d <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")), any.missing = FALSE)
  contrast <- as.vector(contrast)
  switch(object$method,
    "Satterthwaite" = h_df_1d_sat(object, contrast),
    "Kenward-Roger" = h_df_1d_kr(object, contrast),
    "Residual" = h_df_1d_res(object, contrast),
    "Between-Within" = h_df_1d_bw(object, contrast),
    stop("Unrecognized degrees of freedom method: ", object$method)
  )
}


#' Calculation of Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#' Calculates the estimate, standard error, degrees of freedom,
#' t statistic and p-value for one-dimensional contrast, depending on the method
#' used in [mmrm()].
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`matrix`)\cr numeric contrast matrix, if given a `numeric`
#'   then this is coerced to a row vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#' @export
#'
#' @examples
#' object <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' contrast <- matrix(data = 0, nrow = 2, ncol = length(object$beta_est))
#' contrast[1, 2] <- contrast[2, 3] <- 1
#' df_md(object, contrast)
df_md <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, any.missing = FALSE)
  if (!is.matrix(contrast)) {
    contrast <- matrix(contrast, ncol = length(contrast))
  }
  assert_matrix(contrast, ncols = length(component(object, "beta_est")))
  switch(object$method,
    "Satterthwaite" = h_df_md_sat(object, contrast),
    "Kenward-Roger" = h_df_md_kr(object, contrast),
    "Residual" = h_df_md_res(object, contrast),
    "Between-Within" = h_df_md_bw(object, contrast),
    stop("Unrecognized degrees of freedom method: ", object$method)
  )
}

#' Creating T-Statistic Test Results For One-Dimensional Contrast
#'
#' @description Creates a list of results for one-dimensional contrasts using
#' a t-test statistic and the given degrees of freedom.
#'
#' @inheritParams df_1d
#' @param df (`number`)\cr degrees of freedom for the one-dimensional contrast.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_test_1d <- function(object,
                      contrast,
                      df) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  assert_number(df, lower = .Machine$double.xmin)

  est <- sum(contrast * component(object, "beta_est"))
  var <- h_quad_form_vec(contrast, component(object, "beta_vcov"))
  se <- sqrt(var)
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

#' Creating F-Statistic Test Results For Multi-Dimensional Contrast
#'
#' @description Creates a list of results for multi-dimensional contrasts using
#' an F-test statistic and the given degrees of freedom.
#'
#' @inheritParams df_md
#' @param contrast (`matrix`)\cr numeric contrast matrix.
#' @param df (`number`)\cr denominator degrees of freedom for the multi-dimensional contrast.
#' @param f_stat_factor (`number`)\cr optional scaling factor on top of the standard F-statistic.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_test_md <- function(object,
                      contrast,
                      df,
                      f_stat_factor = 1) {
  assert_class(object, "mmrm")
  assert_matrix(contrast, ncols = length(component(object, "beta_est")))
  num_df <- nrow(contrast)
  assert_number(df, lower = .Machine$double.xmin)
  assert_number(f_stat_factor, lower = .Machine$double.xmin)

  prec_contrast <- solve(h_quad_form_mat(contrast, component(object, "beta_vcov")))
  contrast_est <- component(object, "beta_est") %*% t(contrast)
  f_statistic <- as.numeric(f_stat_factor / num_df * h_quad_form_mat(contrast_est, prec_contrast))
  p_val <- stats::pf(
    q = f_statistic,
    df1 = num_df,
    df2 = df,
    lower.tail = FALSE
  )

  list(
    num_df = num_df,
    denom_df = df,
    f_stat = f_statistic,
    p_val = p_val
  )
}
