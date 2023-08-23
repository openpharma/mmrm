#' Obtain List of Jacobian Matrix Entries for Covariance Matrix
#'
#' @description Obtain the Jacobian matrices given the covariance function and variance parameters.
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta_est (`numeric`)\cr variance parameters point estimate.
#' @param beta_vcov (`matrix`)\cr vairance covariance matrix of coefficients.
#'
#' @return List with one element per variance parameter containing a matrix
#'   of the same dimensions as the covariance matrix. The values are the derivatives
#'   with regards to this variance parameter.
#'
#' @keywords internal
h_jac_list <- function(tmb_data,
                       theta_est,
                       beta_vcov) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_numeric(theta_est)
  assert_matrix(beta_vcov)
  .Call(`_mmrm_get_jacobian`, PACKAGE = "mmrm", tmb_data, theta_est, beta_vcov)
}

#' Quadratic Form Calculations
#'
#' @description These helpers are mainly for easier readability and slightly better efficiency
#' of the quadratic forms used in the Satterthwaite calculations.
#'
#' @param center (`matrix`)\cr square numeric matrix with the same dimensions as
#'   `x` as the center of the quadratic form.
#'
#' @name h_quad_form
NULL

#' @describeIn h_quad_form calculates the number `vec %*% center %*% t(vec)`
#'   as a numeric (not a matrix).
#'
#' @param vec (`numeric`)\cr interpreted as a row vector.
#'
#' @keywords internal
h_quad_form_vec <- function(vec, center) {
  vec <- as.vector(vec)
  assert_numeric(vec, any.missing = FALSE)
  assert_matrix(
    center,
    mode = "numeric",
    any.missing = FALSE,
    nrows = length(vec),
    ncols = length(vec)
  )

  sum(vec * (center %*% vec))
}

#' @describeIn h_quad_form calculates the quadratic form `mat %*% center %*% t(mat)`
#'   as a matrix, the result is square and has dimensions identical to the number
#'   of rows in `mat`.
#'
#' @param mat (`matrix`)\cr numeric matrix to be multiplied left and right of
#'   `center`, therefore needs to have as many columns as there are rows and columns
#'   in `center`.
#'
#' @keywords internal
h_quad_form_mat <- function(mat, center) {
  assert_matrix(mat, mode = "numeric", any.missing = FALSE, min.cols = 1L)
  assert_matrix(
    center,
    mode = "numeric",
    any.missing = FALSE,
    nrows = ncol(center),
    ncols = ncol(center)
  )
  mat %*% tcrossprod(center, mat)
}

#' Computation of a Gradient Given Jacobian and Contrast Vector
#'
#' @description Computes the gradient of a linear combination of `beta` given the Jacobian matrix and
#' variance parameters.
#'
#' @param jac_list (`list`)\cr Jacobian list produced e.g. by [h_jac_list()].
#' @param contrast (`numeric`)\cr contrast vector, which needs to have the
#'   same number of elements as there are rows and columns in each element of
#'   `jac_list`.
#'
#' @return Numeric vector which contains the quadratic forms of each element of
#'   `jac_list` with the `contrast` vector.
#'
#' @keywords internal
h_gradient <- function(jac_list, contrast) {
  assert_list(jac_list)
  assert_numeric(contrast)

  vapply(
    jac_list,
    h_quad_form_vec,
    vec = contrast,
    numeric(1L)
  )
}

#' Calculation of Satterthwaite Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Used in [df_1d()] if method is
#' "Satterthwaite".
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @keywords internal
h_df_1d_sat <- function(object, contrast) {
  assert_class(object, "mmrm")
  contrast <- as.numeric(contrast)
  assert_numeric(contrast, len = length(component(object, "beta_est")))

  df <- if (identical(object$vcov, "Asymptotic")) {
    grad <- h_gradient(component(object, "jac_list"), contrast)
    v_num <- 2 * h_quad_form_vec(contrast, component(object, "beta_vcov"))^2
    v_denom <- h_quad_form_vec(grad, component(object, "theta_vcov"))
    v_num / v_denom
  } else if (object$vcov %in% c("Empirical", "Empirical-Jackknife", "Empirical-Bias-Reduced")) {
    contrast_matrix <- Matrix::.bdiag(rep(list(matrix(contrast, nrow = 1)), component(object, "n_subjects")))
    contrast_matrix <- as.matrix(contrast_matrix)
    g_matrix <- h_quad_form_mat(contrast_matrix, object$empirical_df_mat)
    h_tr(g_matrix)^2 / sum(g_matrix^2)
  }

  h_test_1d(object, contrast, df)
}

#' Calculating Denominator Degrees of Freedom for the Multi-Dimensional Case
#'
#' @description Calculates the degrees of freedom for multi-dimensional contrast.
#'
#' @param t_stat_df (`numeric`)\cr `n` t-statistic derived degrees of freedom.
#'
#' @return Usually the calculation is returning `2 * E / (E - n)` where
#'   `E` is the sum of `t / (t - 2)` over all `t_stat_df` values `t`.
#'
#' @note If the input values are two similar to each other then just the average
#'   of them is returned. If any of the inputs is not larger than 2 then 2 is
#'   returned.
#'
#' @keywords internal
h_md_denom_df <- function(t_stat_df) {
  assert_numeric(t_stat_df, min.len = 1L, lower = .Machine$double.xmin, any.missing = FALSE)

  if (test_scalar(t_stat_df)) {
    t_stat_df
  } else if (all(abs(diff(t_stat_df)) < sqrt(.Machine$double.eps))) {
    mean(t_stat_df)
  } else if (any(t_stat_df <= 2)) {
    2
  } else {
    e <- sum(t_stat_df / (t_stat_df - 2))
    2 * e / (e - (length(t_stat_df)))
  }
}

#' Creating F-Statistic Results from One-Dimensional Contrast
#'
#' @description Creates multi-dimensional result from one-dimensional contrast from [df_1d()].
#'
#' @param object (`mmrm`)\cr model fit.
#' @param contrast (`numeric`)\cr one-dimensional contrast.
#'
#' @return The one-dimensional degrees of freedom are calculated and then
#'   based on that the p-value is calculated.
#'
#' @keywords internal
h_df_md_from_1d <- function(object, contrast) {
  res_1d <- h_df_1d_sat(object, contrast)
  list(
    num_df = 1,
    denom_df = res_1d$df,
    f_stat = res_1d$t_stat^2,
    p_val = stats::pf(q = res_1d$t_stat^2, df1 = 1, df2 = res_1d$df, lower.tail = FALSE)
  )
}

#' Calculation of Satterthwaite Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Used in [df_md()] if method is "Satterthwaite".
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`matrix`)\cr numeric contrast matrix, if given a `numeric`
#'   then this is coerced to a row vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#' @keywords internal
h_df_md_sat <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_matrix(contrast, mode = "numeric", any.missing = FALSE, ncols = length(component(object, "beta_est")))
  # Early return if we are in the one-dimensional case.
  if (identical(nrow(contrast), 1L)) {
    return(h_df_md_from_1d(object, contrast))
  }

  contrast_cov <- h_quad_form_mat(contrast, component(object, "beta_vcov"))
  eigen_cont_cov <- eigen(contrast_cov)
  eigen_cont_cov_vctrs <- eigen_cont_cov$vectors
  eigen_cont_cov_vals <- eigen_cont_cov$values

  eps <- sqrt(.Machine$double.eps)
  tol <- max(eps * eigen_cont_cov_vals[1], 0)
  rank_cont_cov <- sum(eigen_cont_cov_vals > tol)
  assert_number(rank_cont_cov, lower = .Machine$double.xmin)
  rank_seq <- seq_len(rank_cont_cov)
  vctrs_cont_prod <- crossprod(eigen_cont_cov_vctrs, contrast)[rank_seq, , drop = FALSE]

  # Early return if rank 1.
  if (identical(rank_cont_cov, 1L)) {
    return(h_df_md_from_1d(object, vctrs_cont_prod))
  }

  t_squared_nums <- drop(vctrs_cont_prod %*% object$beta_est)^2
  t_squared_denoms <- eigen_cont_cov_vals[rank_seq]
  t_squared <- t_squared_nums / t_squared_denoms
  f_stat <- sum(t_squared) / rank_cont_cov
  t_stat_df_nums <- 2 * eigen_cont_cov_vals^2
  t_stat_df <- if (identical(object$vcov, "Asymptotic")) {
    grads_vctrs_cont_prod <- lapply(
      rank_seq,
      function(m) h_gradient(component(object, "jac_list"), contrast = vctrs_cont_prod[m, ])
    )
    t_stat_df_denoms <- vapply(
      grads_vctrs_cont_prod,
      h_quad_form_vec,
      center = component(object, "theta_vcov"),
      numeric(1)
    )
    t_stat_df_nums / t_stat_df_denoms
  } else {
    vapply(
      rank_seq,
      function(m) {
        contrast_matrix <- Matrix::.bdiag(
          rep(list(vctrs_cont_prod[m, , drop = FALSE]), component(object, "n_subjects"))
        )
        contrast_matrix <- as.matrix(contrast_matrix)
        g_matrix <- h_quad_form_mat(contrast_matrix, object$empirical_df_mat)
        h_tr(g_matrix)^2 / sum(g_matrix^2)
      },
      FUN.VALUE = 0
    )
  }
  denom_df <- h_md_denom_df(t_stat_df)

  list(
    num_df = rank_cont_cov,
    denom_df = denom_df,
    f_stat = f_stat,
    p_val = stats::pf(q = f_stat, df1 = rank_cont_cov, df2 = denom_df, lower.tail = FALSE)
  )
}
