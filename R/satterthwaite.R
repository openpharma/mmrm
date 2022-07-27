#' Covariance Matrix for Coefficients Given Variance Parameters
#'
#' @param model (`mmrm_tmb`)\cr initial model fit.
#'
#' @return Function with argument `theta` that calculates the covariance matrix
#'   for the coefficient vector `beta`.
#'
#' @keywords internal
h_covbeta_fun <- function(model) {
  assert_class(model, "mmrm_tmb")

  function(theta) {
    reported <- model$tmb_object$report(theta)
    reported$beta_vcov
  }
}

#' Formatting a Column from Jacobian Matrix as Matrix
#'
#' @param jac_matrix (`matrix`)\cr full Jacobian matrix.
#' @param col (`int`)\cr column index.
#'
#' @return The column `col` of `jac_matrix` as a square matrix.
#'   Here the values in the column are used to fill the result column
#'   by column.
#'
#' @keywords internal
h_jac_col_as_matrix <- function(jac_matrix, col) {
  assert_int(col)
  assert_matrix(jac_matrix, min.cols = col)
  p <- sqrt(nrow(jac_matrix))
  assert_integerish(p)

  jac_col <- jac_matrix[, col, drop = TRUE]
  matrix(jac_col, nrow = p, ncol = p)
}

#' Obtain List of Jacobian Matrix Entries for Covariance Matrix
#'
#' @param covbeta_fun (`function`)\cr function calculating the covariance
#'   matrix of coefficients given variance parameters (`theta`), see
#'   [h_covbeta_fun()] to obtain this from a `mmrm_tmb` object.
#' @param theta_est (`numeric`)\cr variance parameters point estimate.
#'
#' @return List with one element per variance parameter containing a matrix
#'   of the same dimensions as the covariance matrix. The values are the derivatives
#'   with regards to this variance parameter.
#'
#' @keywords internal
h_jac_list <- function(covbeta_fun,
                       theta_est) {
  assert_function(covbeta_fun, args = "theta")
  assert_numeric(theta_est, any.missing = FALSE, min.len = 1L)

  jac_matrix <- numDeriv::jacobian(
    func = covbeta_fun,
    x = theta_est,
    method = "Richardson"
  )
  lapply(
    seq_len(ncol(jac_matrix)),
    FUN = h_jac_col_as_matrix,
    jac_matrix = jac_matrix
  )
}

#' Quadratic Form Calculations
#' These helpers are mainly for easier readability and slightly better efficiency
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

#' Creating Results List for One-Dimensional Contrast
#'
#' @param est (`number`)\cr estimate.
#' @param var (`number`)\cr variance of estimate.
#' @param v_num (`number`)\cr numerator for Satterthwaite d.f.
#' @param v_denom (`number`)\cr denominator for Satterthwaite d.f.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_df_1d_list <- function(est,
                         var,
                         v_num,
                         v_denom) {
  assert_number(est)
  assert_number(var, lower = .Machine$double.xmin)
  assert_number(v_num, lower = .Machine$double.xmin)
  assert_number(v_denom, lower = .Machine$double.xmin)

  se <- sqrt(var)
  t_stat <- est / se
  df <- v_num / v_denom
  p_val <- 2 * stats::pt(q = abs(t_stat), df = df, lower.tail = FALSE)

  list(
    est = est,
    se = se,
    df = df,
    t_stat = t_stat,
    p_val = p_val
  )
}

#' Calculation of Satterthwaite Degrees of Freedom for One-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
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
  assert_numeric(contrast, any.missing = FALSE)

  contrast <- as.vector(contrast)
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  est <- sum(contrast * component(object, "beta_est"))
  var <- h_quad_form_vec(contrast, component(object, "beta_vcov"))
  grad <- h_gradient(component(object, "jac_list"), contrast)

  v_num <- 2 * var^2
  v_denom <- h_quad_form_vec(grad, component(object, "theta_vcov"))

  h_df_1d_list(
    est = est,
    var = var,
    v_num = v_num,
    v_denom = v_denom
  )
}

#' Calculating Denominator Degrees of Freedom for the Multi-Dimensional Case
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
    E <- sum(t_stat_df / (t_stat_df - 2))
    2 * E / (E - (length(t_stat_df)))
  }
}

#' Creating Results List for Multi-Dimensional Contrast
#'
#' @param f_stat (`number`)\cr F-statistic.
#' @param num_df (`number`)\cr numerator degrees of freedom.
#' @param denom_df (`number`)\cr denominator degrees of freedom.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_df_md_list <- function(f_stat, num_df, denom_df) {
  assert_number(f_stat, lower = .Machine$double.xmin)
  assert_number(num_df, lower = 1)
  assert_number(denom_df, lower = 2)

  p_val <- stats::pf(
    q = f_stat,
    df1 = num_df,
    df2 = denom_df,
    lower.tail = FALSE
  )

  list(
    num_df = num_df,
    denom_df = denom_df,
    f_stat = f_stat,
    p_val = p_val
  )
}

#' Creating F-Statistic Results from One-Dimensional Contrast
#'
#' @param object (`mmrm`)\cr model fit.
#' @param contrast (`numeric`)\cr one-dimensional contrast.
#'
#' @return The one-dimensional results are calculated and then returned as per
#'   [h_df_md_list()].
#'
#' @keywords internal
h_df_md_from_1d <- function(object, contrast) {
  res_1d <- df_1d(object, contrast)
  h_df_md_list(
    f_stat = res_1d$t_stat^2,
    num_df = 1,
    denom_df = res_1d$df
  )
}

#' Calculation of Satterthwaite Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`matrix`)\cr numeric contrast matrix, if given a `numeric`
#'   then this is coerced to a row vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
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
  grads_vctrs_cont_prod <- lapply(rank_seq, function(m) h_gradient(component(object, "jac_list"), contrast = vctrs_cont_prod[m, ]))
  t_stat_df_nums <- 2 * eigen_cont_cov_vals^2
  t_stat_df_denoms <- vapply(grads_vctrs_cont_prod, h_quad_form_vec, center = component(object, "theta_vcov"), numeric(1))
  t_stat_df <- t_stat_df_nums / t_stat_df_denoms
  denom_df <- h_md_denom_df(t_stat_df)

  h_df_md_list(
    f_stat = f_stat,
    num_df = rank_cont_cov,
    denom_df = denom_df
  )
}
