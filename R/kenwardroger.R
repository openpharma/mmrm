#' Obtain Kenward-Roger Adjustment Components
#'
#' @description Obtains the components needed downstream for the computation of Kenward-Roger degrees of freedom.
#' Used in [mmrm()] fitting if method is "Kenward-Roger" or "Kenward-Roger-Linear".
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta (`numeric`)\cr theta estimate.
#'
#' @details the function returns a named list, \eqn{P}, \eqn{Q} and \eqn{R}, which corresponds to the
#' paper in 1997. The matrices are stacked in columns so that \eqn{P}, \eqn{Q} and \eqn{R} has the same
#' column number(number of beta parameters). The number of rows, is dependent on
#' the total number of theta and number of groups, if the fit is a grouped mmrm.
#' For \eqn{P} matrix, it is stacked sequentially. For \eqn{Q} and \eqn{R} matrix, it is stacked so
#' that the \eqn{Q_{ij}} and \eqn{R_{ij}} is stacked from \eqn{j} then to \eqn{i}, i.e. \eqn{R_{i1}}, \eqn{R_{i2}}, etc.
#' \eqn{Q} and \eqn{R} only contains intra-group results and inter-group results should be all zero matrices
#' so they are not stacked in the result.
#'
#' @return Named list with elements:
#' - `P`: `matrix` of \eqn{P} component.
#' - `Q`: `matrix` of \eqn{Q} component.
#' - `R`: `matrix` of \eqn{R} component.
#'
#' @keywords internal
h_get_kr_comp <- function(tmb_data, theta) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_class(theta, "numeric")
  .Call(`_mmrm_get_pqr`, PACKAGE = "mmrm", tmb_data, theta)
}


#' Calculation of Kenward-Roger Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Calculates the Kenward-Roger degree of freedom, F statistic and p value for multi-dimensional contrast.
#' Used in [df_md()] if method is "Kenward-Roger" or "Kenward-Roger-Linear".
#'
#' @param object (`mmrm`)\cr object.
#' @param contrast (`matrix`)\cr contrast matrix.
#' @param linear (`logical`)\cr whether to use linear Kenward-Roger approximation.
#'
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#'
#' @keywords internal
h_df_md_kr <- function(object, contrast, linear = FALSE) {
  assert_class(object, "mmrm")
  assert_flag(linear)
  assert_matrix(contrast, mode = "numeric", any.missing = FALSE, ncols = length(component(object, "beta_est")))
  if (component(object, "reml") != 1) {
    stop("Kenward-Roger is only for REML")
  }
  kr_comp <- object$kr_comp
  w <- component(object, "theta_vcov")
  v_adj <- object$beta_vcov_adj
  df <- h_kr_df(v0 = object$beta_vcov, l = contrast, w = w, p = kr_comp$P)
  prec_contrast <- solve(h_quad_form_mat(contrast, v_adj))
  contrast_est <- component(object, "beta_est") %*% t(contrast)
  f_statistic <- 1 / nrow(contrast) * h_quad_form_mat(contrast_est, prec_contrast)
  f_star <- f_statistic * df$lambda
  list(
    num_df = nrow(contrast),
    denom_df = df$m,
    f_stat = f_star[1, 1],
    p_val = stats::pf(f_star[1, 1], nrow(contrast), df$m, lower.tail = FALSE)
  )
}

#' Calculation of Kenward-Roger Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Calculates the estimate, adjusted standard error, Kenward-Roger degree of freedom,
#' t statistic and p-value for one-dimensional contrast. Used in [df_1d()] if method is
#' "Kenward-Roger" or "Kenward-Roger-Linear".
#'
#' @param object (`mmrm`)\cr object created by [mmrm()] with Kenward-Roger(-Linear) method.
#' @param contrast (`numeric`)\cr contrast vector.
#' @param linear (`logical`)\cr whether to use linear Kenward-Roger approximation.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#'
#' @keywords internal
h_df_1d_kr <- function(object, contrast, linear = FALSE) {
  assert_class(object, "mmrm")
  assert_flag(linear)
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  if (component(object, "reml") != 1) {
    stop("Kenward-Roger is only for REML!")
  }
  est <- sum(contrast * component(object, "beta_est"))
  df <- h_kr_df(
    v0 = object$beta_vcov,
    l = matrix(contrast, nrow = 1),
    w = component(object, "theta_vcov"),
    p = object$kr_comp$P
  )
  se <- sqrt(h_quad_form_vec(contrast, object$beta_vcov_adj))

  t_stat <- abs(est) / se
  p_val <- stats::pt(t_stat, df$m, lower.tail = FALSE) * 2

  list(
    est = est,
    se = se,
    df = df$m,
    t_stat = t_stat,
    p_val = p_val
  )
}

#' Obtain the Adjusted Kenward-Roger Degree of Freedom
#'
#' @description Obtains the adjusted Kenward-Roger degree of freedom and F statistic scale parameter.
#' Used in [h_df_md_kr()] or [h_df_1d_kr].
#'
#' @param v0 (`matrix`)\cr unadjusted covariance matrix.
#' @param l (`matrix`)\cr linear combination matrix.
#' @param w (`matrix`)\cr hessian matrix.
#' @param p (`matrix`)\cr P matrix from [h_get_kr_comp()].
#'
#' @return Named list with elements:
#' - `m`: `numeric` degree of freedom.
#' - `lambda`: `numeric` F statistic scale parameter.
#'
#' @keywords internal
h_kr_df <- function(v0, l, w, p) {
  n_beta <- ncol(v0)
  assert_matrix(v0, ncols = n_beta, nrows = n_beta)
  assert_matrix(l, ncols = n_beta)
  n_theta <- ncol(w)
  assert_matrix(w, ncols = n_theta, nrows = n_theta)
  n_visits <- ncol(p)
  assert_matrix(p, nrows = n_visits * n_theta)
  # see vignettes/kenward.Rmd#279
  slvol <- solve(h_quad_form_mat(l, v0))
  m <- h_quad_form_mat(t(l), slvol)
  nl <- nrow(l)
  mv0 <- m %*% v0
  pl <- lapply(seq_len(nrow(p) / ncol(p)), function(x) {
    ii <- (x - 1) * ncol(p) + 1
    jj <- x * ncol(p)
    p[ii:jj, ]
  })
  mv0pv0 <- lapply(pl, function(x) {
    mv0 %*% x %*% v0
  })
  a1 <- 0
  a2 <- 0
  # see vignettes/kenward.Rmd#283
  for (i in seq_len(length(pl))) {
    for (j in seq_len(length(pl))) {
      a1 <- a1 + w[i, j] * h_tr(mv0pv0[[i]]) * h_tr(mv0pv0[[j]])
      a2 <- a2 + w[i, j] * h_tr(mv0pv0[[i]] %*% mv0pv0[[j]])
    }
  }
  b <- 1 / (2 * nl) * (a1 + 6 * a2)
  e <- 1 + a2 / nl
  e_star <- 1 / (1 - a2 / nl)
  g <- ((nl + 1) * a1 - (nl + 4) * a2) / ((nl + 2) * a2)
  denom <- (3 * nl + 2 - 2 * g)
  c1 <- g / denom
  c2 <- (nl - g) / denom
  c3 <- (nl + 2 - g) / denom
  v_star <- 2 / nl * (1 + c1 * b) / (1 - c2 * b)^2 / (1 - c3 * b)
  rho <- v_star / (2 * e_star^2)
  m <- 4 + (nl + 2) / (nl * rho - 1)
  lambda <- m / (e_star * (m - 2))
  list(m = m, lambda = lambda)
}

#' Obtain the Adjusted Covariance Matrix
#'
#' @description Obtains the Kenward-Roger adjusted covariance matrix for the
#'   coefficient estimates.
#' Used in [mmrm()] fitting if method is "Kenward-Roger" or "Kenward-Roger-Linear".
#'
#' @param v (`matrix`)\cr unadjusted covariance matrix.
#' @param w (`matrix`)\cr hessian matrix.
#' @param p (`matrix`)\cr P matrix from [h_get_kr_comp()].
#' @param q (`matrix`)\cr Q matrix from [h_get_kr_comp()].
#' @param r (`matrix`)\cr R matrix from [h_get_kr_comp()].
#' @param linear (`logical`)\cr whether to use linear Kenward-Roger approximation.
#'
#' @return The matrix of adjusted covariance matrix.
#'
#' @keywords internal
h_var_adj <- function(v, w, p, q, r, linear = FALSE) {
  assert_flag(linear)
  n_beta <- ncol(v)
  assert_matrix(v, nrows = n_beta)
  n_theta <- ncol(w)
  assert_matrix(w, nrows = n_theta)
  n_visits <- ncol(p)
  theta_per_group <- nrow(q) / nrow(p)
  n_groups <- n_theta / theta_per_group
  assert_matrix(p, nrows = n_theta * n_visits)
  assert_matrix(q, nrows = theta_per_group^2 * n_groups * n_visits, ncols = n_visits)
  assert_matrix(r, nrows = theta_per_group^2 * n_groups * n_visits, ncols = n_visits)
  if (linear) {
    r <- matrix(0, nrow = nrow(r), ncol = ncol(r))
  }

  # see vignettes/kenward.Rmd#131
  ret <- v
  for (i in seq_len(n_theta)) {
    for (j in seq_len(n_theta)) {
      gi <- ceiling(i / theta_per_group)
      gj <- ceiling(j / theta_per_group)
      iid <- (i - 1) * n_beta + 1
      jid <- (j - 1) * n_beta + 1
      ii <- i - (gi - 1) * theta_per_group
      jj <- j - (gi - 1) * theta_per_group
      ijid <- ((ii - 1) * theta_per_group + jj - 1) * n_beta + (gi - 1) * n_beta * theta_per_group^2 + 1
      if (gi != gj) {
        ret <- ret + 2 * w[i, j] * v %*% (-p[iid:(iid + n_beta - 1), ] %*% v %*% p[jid:(jid + n_beta - 1), ]) %*% v
      } else {
        ret <- ret + 2 * w[i, j] * v %*% (
          q[ijid:(ijid + n_beta - 1), ] -
            p[iid:(iid + n_beta - 1), ] %*% v %*% p[jid:(jid + n_beta - 1), ] -
            1 / 4 * r[ijid:(ijid + n_beta - 1), ]
        ) %*% v
      }
    }
  }
  ret
}
