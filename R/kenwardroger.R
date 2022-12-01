#' obtain kr component
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta (`numeric`)\cr numeric vector of the theta estimate.
#' @details the function retuns a named list, \eqn{P}, \eqn{Q} and \eqn{R}, which corresponds to the
#' paper in 1997. The matrices are stacked in columns so that \eqn{P}, \eqn{Q} and \eqn{R} has the same
#' column number(number of beta parameters). The number of rows, is dependent on
#' the total number of theta and number of groups, if the fit is a grouped mmrm.
#' For \eqn{P} matrix, it is stacked sequentially. For \eqn{Q} and \eqn{R} matrix, it is stacked so
#' that the \eqn{Q_{ij}} and \eqn{R_{ij}} is stacked from \eqn{j} then to \eqn{i}, i.e. \eqn{R_{i1}}, \eqn{R_{i2}}, etc.
#' \eqn{Q} and \eqn{R} only contains within group results and intra-group results should be all zero matrices
#' so they are not stacked in the result.
#' @return Named list with elements:
#' - `P`: `matrix` of P component.
#' - `Q`: `matrix` of Q component.
#' - `R`: `matrix` of R component.
#' @keywords internal
h_get_kr_comp <- function(tmb_data, theta) {
  .Call(`_mmrm_get_pqr`, PACKAGE = "mmrm", tmb_data, theta)
}


#' Calculation of Kenward-Roger Degrees of Freedom for Multi-Dimensional Contrast
#' @param object `mmrm` object
#' @param contrast `matrix` contrast matrix
#' @param linear `logical`\cr whether to use linear Kenward-Roger approximation
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#' @keywords internal
df_md_kr <- function(object, contrast, linear = FALSE) {
  if (object$tmb_data$reml != 1) {
    stop("Kenward-Roger is only for REML!")
  }
  kr_comp <- object$kr_comp
  w <- solve(object$tmb_obj$he(object$theta_est))
  v_adj <- h_var_adj(object$beta_vcov, w, kr_comp$P, kr_comp$Q, kr_comp$R, linear = linear)
  df <- h_kr_df(object$beta_vcov, contrast, w, kr_comp$P)
  f_statistic <- 1 / nrow(contrast) * object$beta_est %*% t(contrast) %*% solve(contrast %*% v_adj %*% t(contrast)) %*% contrast %*% object$beta_est
  f_star <- f_statistic * df$lambda
  ret <- list(
    num_df = nrow(contrast),
    num_df = nrow(contrast),
    denom_df = df$m,
    f_stat = f_star[1, 1],
    p_val = pf(f_star[1, 1], nrow(contrast), df$m, lower.tail = FALSE)
  )
  ret$sqrt <- sqrt(contrast %*% v_adj %*% t(contrast))
  ret
}

#' Calculation of Kenward-Roger Degrees of Freedom for One-Dimensional Contrast
#' @param object `mmrm` object
#' @param contrast contrast vector
#' @param linear `logical`\cr whether to use linear Kenward-Roger approximation
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @keywords internal
df_1d_kr <- function(object, contrast, linear = FALSE) {
  if (object$tmb_data$reml != 1) {
    stop("Kenward-Roger is only for REML!")
  }
  assert_numeric(contrast, len = length(component(object, "beta_est")))
  est <- sum(contrast * component(object, "beta_est"))
  kr_comp <- object$kr_comp
  w <- solve(object$tmb_obj$he(object$theta_est))
  v_adj <- h_var_adj(object$beta_vcov, w, kr_comp$P, kr_comp$Q, kr_comp$R, linear = linear)
  df <- h_kr_df(object$beta_vcov, matrix(contrast, nrow = 1), w, kr_comp$P)
  se <- sqrt(contrast %*% v_adj %*% contrast)[1, 1]
  
  ret <- list(
    est = est,
    se = se,
    df = df$m
  )
  t_stat = est ^ 2 / se
  ret$t_stat <- t_stat
  ret$pval <- pt(t_stat, df$m, lower.tail = FALSE)
  ret
}

#' obtain the adjusted Kenward-Roger degree of freedom
#' @param v0 unadjusted covariance matrix
#' @param l linear combination matrix
#' @param w hessian matrix
#' @param p P matrix from `h_get_kr_comp`
#' @return List with `m` and `lambda`
#' @keywords internal
h_kr_df <- function(v0, l, w, p) {
  theta <- t(l) %*% solve(l %*% v0 %*% t(l)) %*% l
  nl <- nrow(l)
  thetav0 <- theta %*% v0
  pl <- lapply(seq_len(nrow(p) / ncol(p)), function(x) {
    ii <- (x - 1) * ncol(p) + 1
    jj <- x * ncol(p)
    p[ii:jj, ]
  })
  thetav0pv0 <- lapply(pl, function(x) {thetav0 %*% x %*% v0})
  a1 <- 0
  a2 <- 0
  for (i in seq_len(length(pl))) {
    for (j in seq_len(length(pl))) {
      a1 <- a1 + w[i, j] * h_tr(thetav0pv0[[i]]) * h_tr(thetav0pv0[[j]])
      a2 <- a2 + w[i, j] * h_tr(thetav0pv0[[i]] %*% thetav0pv0[[j]])
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
  m <- 4 + (nl + 2)  / (nl * rho - 1)
  lambda <- m / (e_star * (m - 2))
  list(m = m, lambda = lambda)
}

#' obtain the adjusted covariance matrix
#' @param v unadjusted covariance matrix
#' @param w hessian matrix
#' @param p P matrix from `h_get_kr_comp`
#' @param q Q matrix from `h_get_kr_comp`
#' @param r R matrix from `h_get_kr_comp`
#' @param linear `logical`\cr whether to use linear Kenward-Roger approximation
#' @return matrix of adjusted covariance matrix
#' @keywords internal
h_var_adj <- function(v, w, p, q, r, linear = FALSE) {
  if (linear) {
    r[] <- 0
  }
  dr <- ncol(v)
  n_theta <- ncol(w)
  n_groups <- n_theta / dr
  ret <- v
  for (gi in seq_len(n_groups)) {
    for (gj in seq_len(n_groups)) {
      for (i in seq_len(dr)) {
        for (j in seq_len(dr)) {
          iid <- (i - 1) * dr + (gi - 1) * dr ^ 2 + 1
          jid <- (j - 1) * dr + (gj - 1) * dr ^ 2 + 1
          ijid <- ((i - 1) * dr + j - 1) * dr + (gi - 1) * dr ^ 3 + 1
          if (gi != gj) {
            ret <- ret + 2 * w[i, j] * v %*% (-p[iid:(iid + dr - 1), ] %*% v %*% p[jid:(jid + dr - 1), ]) %*% v
          } else {
            ret <- ret + 2 * w[i, j] * v %*% (
              q[ijid:(ijid + dr - 1),] -
              p[iid:(iid + dr - 1), ] %*% v %*% p[jid:(jid + dr - 1), ] -
              1 / 4 * r[ijid:(ijid + dr - 1), ]
            ) %*% v
          }
        }
      }
    }
  }
  ret
}
