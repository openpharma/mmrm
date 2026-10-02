#' Obtain Kenward-Roger Adjustment Components
#'
#' @description Obtains the components needed downstream for the computation of Kenward-Roger degrees of freedom.
#' Used in [mmrm()] fitting if method is "Kenward-Roger".
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta (`numeric`)\cr theta estimate.
#' @param linear (`flag`)\cr whether to omit second derivatives and the R component.
#' @param w (`matrix` or `NULL`)\cr covariance of the covariance parameters.
#'   Supply with `linear = TRUE` to contract Q without constructing its blocks.
#'
#' @details the function returns a named list, \eqn{P}, \eqn{Q} and \eqn{R}, which corresponds to the
#' paper in 1997. The matrices are stacked in columns so that \eqn{P}, \eqn{Q} and \eqn{R} has the same
#' column number(number of beta parameters). The number of rows, is dependent on
#' the total number of theta and number of groups, if the fit is a grouped mmrm.
#' For \eqn{P} matrix, it is stacked sequentially. For \eqn{Q} and \eqn{R} matrix, it is stacked so
#' that the \eqn{Q_{ij}} and \eqn{R_{ij}} is stacked from \eqn{j} then to \eqn{i}, i.e. \eqn{R_{i1}}, \eqn{R_{i2}}, etc.
#' \eqn{Q} and \eqn{R} only contains intra-group results and inter-group results should be all zero matrices
#' so they are not stacked in the result. Supplying `w` for linear KR instead
#' retains `P` and contracts Q into the single matrix `S_Q`, without storing Q blocks.
#'
#' @return Named list with elements:
#' - `P`: `matrix` of \eqn{P} component.
#' - `Q`: `matrix` of \eqn{Q} component.
#' - `R`: `matrix` of \eqn{R} component, or `NULL` when `linear = TRUE`.
#' - `S_Q`: contracted Q sum when `w` is supplied; `Q` and `R` are then `NULL`.
#'
#' @keywords internal
h_get_kr_comp <- function(tmb_data, theta, linear = FALSE, w = NULL) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_class(theta, "numeric")
  assert_flag(linear)
  if (!is.null(w)) {
    assert_true(linear)
    assert_matrix(w, mode = "numeric", nrows = length(theta), ncols = length(theta), any.missing = FALSE)
    assert_true(all(is.finite(w)))
    assert_true(isSymmetric(w, tol = 1e-8, tol1 = 1e-8))
  }
  .Call(`_mmrm_get_pqr`, PACKAGE = "mmrm", tmb_data, theta, linear, w)
}

#' Calculation of Kenward-Roger Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Used in [df_md()] if method is "Kenward-Roger" or "Kenward-Roger-Linear".
#'
#' @inheritParams h_df_md_sat
#' @inherit h_df_md_sat return
#' @keywords internal
h_df_md_kr <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_matrix(
    contrast,
    mode = "numeric",
    any.missing = FALSE,
    ncols = length(component(object, "beta_est"))
  )
  if (component(object, "reml") != 1) {
    stop("Kenward-Roger is only for REML")
  }
  kr_comp <- object$kr_comp
  w <- component(object, "theta_vcov")
  v_adj <- object$beta_vcov_adj
  df <- h_kr_df(v0 = object$beta_vcov, l = contrast, w = w, p = kr_comp$P)

  h_test_md(object, contrast, df = df$m, f_stat_factor = df$lambda)
}

#' Calculation of Kenward-Roger Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Used in [df_1d()] if method is
#' "Kenward-Roger" or "Kenward-Roger-Linear".
#'
#' @inheritParams h_df_1d_sat
#' @inherit h_df_1d_sat return
#' @keywords internal
h_df_1d_kr <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  if (component(object, "reml") != 1) {
    stop("Kenward-Roger is only for REML!")
  }

  df <- h_kr_df(
    v0 = object$beta_vcov,
    l = matrix(contrast, nrow = 1),
    w = component(object, "theta_vcov"),
    p = object$kr_comp$P
  )

  h_test_1d(object, contrast, df$m)
}

#' Obtain the Adjusted Kenward-Roger degrees of freedom
#'
#' @description Obtains the adjusted Kenward-Roger degrees of freedom and F statistic scale parameter.
#' Used in [h_df_md_kr()] or [h_df_1d_kr].
#'
#' @param v0 (`matrix`)\cr unadjusted covariance matrix.
#' @param l (`matrix`)\cr linear combination matrix.
#' @param w (`matrix`)\cr covariance matrix of the estimated covariance parameters.
#' @param p (`matrix`)\cr P matrix from [h_get_kr_comp()].
#'
#' @return Named list with elements:
#' - `m`: `numeric` degrees of freedom.
#' - `lambda`: `numeric` F statistic scale parameter.
#'
#' @keywords internal
h_kr_df <- function(v0, l, w, p) {
  n_beta <- ncol(v0)
  assert_matrix(v0, ncols = n_beta, nrows = n_beta)
  assert_matrix(l, ncols = n_beta)
  n_theta <- ncol(w)
  assert_matrix(w, ncols = n_theta, nrows = n_theta)
  assert_matrix(p, ncols = n_beta, nrows = n_beta * n_theta)
  nl <- nrow(l)
  lv0 <- l %*% v0
  if (nl == 1L) {
    # A1 = A2 = a' W a: scalar Satterthwaite based on unadjusted variance.
    variance <- sum(lv0 * l)
    if (!isTRUE(variance > 0)) {
      stop("contrast variance must be positive")
    }
    a <- vapply(seq_len(n_theta), function(h) {
      rows <- (h - 1L) * n_beta + seq_len(n_beta)
      sum(lv0 * (lv0 %*% p[rows, , drop = FALSE])) / variance
    }, numeric(1L))
    return(list(m = 2 / h_quad_form_vec(a, w), lambda = 1))
  }

  # Normalize in contrast space: H = T T', Z = T^{-1} L Phi.
  # See vignettes/kenward.Rmd, "Degrees of freedom in contrast space".
  contrast_vcov <- tcrossprod(lv0, l)
  # Cholesky alone can accept a singular hypothesis after roundoff, so check
  # the reciprocal condition number with the same threshold as solve().
  if (rcond(contrast_vcov) < .Machine$double.eps) {
    stop("contrast covariance is numerically singular")
  }
  contrast_chol <- tryCatch(
    chol(contrast_vcov),
    error = function(e) stop("contrast covariance is not positive definite", call. = FALSE)
  )
  z <- forwardsolve(t(contrast_chol), lv0)
  f <- vapply(seq_len(n_theta), function(h) {
    rows <- (h - 1L) * n_beta + seq_len(n_beta)
    as.vector(tcrossprod(z %*% p[rows, , drop = FALSE], z))
  }, numeric(nl * nl))
  traces <- colSums(f[seq.int(1L, nl * nl, by = nl + 1L), , drop = FALSE])
  a1 <- h_quad_form_vec(traces, w)
  a2 <- sum((f %*% w) * f)
  b <- 1 / (2 * nl) * (a1 + 6 * a2)
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
#' @param w (`matrix`)\cr covariance matrix of the estimated covariance parameters.
#' @param p (`matrix`)\cr P matrix from [h_get_kr_comp()].
#' @param q (`matrix` or `NULL`)\cr Q matrix from [h_get_kr_comp()].
#'   May be `NULL` when `s_q` is supplied.
#' @param r (`matrix` or `NULL`)\cr R matrix from [h_get_kr_comp()].
#'   May be `NULL` for the linear approximation.
#' @param linear (`flag`)\cr whether to use linear Kenward-Roger approximation.
#' @param s_q (`matrix` or `NULL`)\cr precontracted sum of Q components.
#'   When supplied for linear KR, `q` and `r` are unused.
#'
#' @return The matrix of adjusted covariance matrix.
#'
#' @keywords internal
h_var_adj <- function(v, w, p, q, r, linear = FALSE, s_q = NULL) {
  assert_flag(linear)
  n_beta <- ncol(v)
  assert_matrix(v, nrows = n_beta)
  n_theta <- ncol(w)
  assert_matrix(w, nrows = n_theta)
  if (!is.null(s_q)) {
    assert_true(linear)
    assert_matrix(p, ncols = n_beta, nrows = n_beta * n_theta)
    assert_matrix(s_q, nrows = n_beta, ncols = n_beta)
    # Flatten each P_h into a column and contract once with the full W.
    # Cross-group W entries contribute here even though Q has no such blocks.
    p_flat <- matrix(vapply(seq_len(n_theta), function(h) {
      rows <- (h - 1L) * n_beta + seq_len(n_beta)
      as.vector(p[rows, , drop = FALSE])
    }, numeric(n_beta * n_beta)), nrow = n_beta * n_beta)
    pw <- p_flat %*% w
    s_p <- matrix(0, n_beta, n_beta)
    for (h in seq_len(n_theta)) {
      s_p <- s_p + matrix(p_flat[, h], n_beta) %*% v %*% matrix(pw[, h], n_beta)
    }
    return(v + 2 * v %*% (s_q - s_p) %*% v)
  }
  n_visits <- ncol(p)
  theta_per_group <- nrow(q) / nrow(p)
  n_groups <- n_theta / theta_per_group
  assert_matrix(p, nrows = n_theta * n_visits)
  assert_matrix(
    q,
    nrows = theta_per_group^2 * n_groups * n_visits,
    ncols = n_visits
  )
  if (!linear) {
    assert_matrix(
      r,
      nrows = theta_per_group^2 * n_groups * n_visits,
      ncols = n_visits
    )
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
      ijid <- ((ii - 1) * theta_per_group + jj - 1) *
        n_beta +
        (gi - 1) * n_beta * theta_per_group^2 +
        1
      if (gi != gj) {
        ret <- ret +
          2 *
            w[i, j] *
            v %*%
              (-p[iid:(iid + n_beta - 1), ] %*%
                v %*%
                p[jid:(jid + n_beta - 1), ]) %*%
              v
      } else {
        adjustment <- q[ijid:(ijid + n_beta - 1), ] -
          p[iid:(iid + n_beta - 1), ] %*% v %*% p[jid:(jid + n_beta - 1), ]
        if (!linear) {
          adjustment <- adjustment - 1 / 4 * r[ijid:(ijid + n_beta - 1), ]
        }
        ret <- ret + 2 * w[i, j] * v %*% adjustment %*% v
      }
    }
  }
  ret
}
