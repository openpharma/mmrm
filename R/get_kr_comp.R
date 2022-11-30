#' obtain kr component
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param theta numeric vector of the theta estimate.
#' @details the function retuns a named list, $P$, $Q$ and $R$, which corresponds to the
#' paper in 1997. The matrices are stacked in columns so that $P$, $Q$ and $R$ has the same
#' column number(number of beta parameters). The number of rows, is dependent on
#' the total number of theta and number of groups, if the fit is a grouped mmrm.
#' For $P$ matrix, it is stacked sequentially. For $Q$ and $R$ matrix, it is stacked so
#' that the $Q_{ij}$ and $R_{ij}$ is stacked from $j$ then to $i$, i.e. $R_{i1}$, $R_{i2}$, etc.
#' $Q$ and $R$ only contains within group results and intra-group results should be all zero matrices
#' so they are not stacked in the result.
#' @keywords internal
h_get_kr_comp <- function(tmb_data, theta) {
  .Call(`_mmrm_get_pqr`, PACKAGE = "mmrm", tmb_data, theta)
}


#' obtain Kenward-Roger degree of freedom
#' @export
#' @param fit `mmrm` fit
#' @param contrast contrast vector (ncol = 1)
#' @examples
#' fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data)
#' contrast <- matrix(c(0,1), ncol = 1)
#' kr(fit, contrast)
kr <- function(fit, contrast, order = 2L) {
  if (fit$tmb_data$reml != 1) {
    stop("Kenward-Roger is only for REML!")
  }
  kr_comp <- fit$kr_comp
  w <- solve(fit$tmb_obj$he(fit$theta_est))
  v_adj <- v_a(fit$beta_vcov, w, kr_comp$P, kr_comp$Q, kr_comp$R, order = order)
  df <- h_kr_df(fit$beta_vcov, contrast, w, kr_comp$P)
  f_statistic <- 1 / nrow(contrast) * fit$beta_est %*% t(contrast) %*% solve(contrast %*% v_adj %*% t(contrast)) %*% contrast %*% fit$beta_est
  f_star <- f_statistic * df$m / (df$m - 1 + nrow(contrast))
  return(list(df = df, v_adj = v_adj, f_star = f_star))
}

#' obtain the adjusted Kenward-Roger degree of freedom
#' @param v0 unadjusted covariance matrix
#' @param l linear combination matrix
#' @param w hessian matrix
#' @param p P matrix from `h_get_kr_comp`
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
      a1 <- a1 + w[i, j] * tr(thetav0pv0[[i]]) * tr(thetav0pv0[[j]])
      a2 <- a2 + w[i, j] * tr(thetav0pv0[[i]] %*% thetav0pv0[[j]])
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
  return(list(m = m, lambda = lambda))
}

#' obtain the adjusted covariance matrix
#' @param v unadjusted covariance matrix
#' @param w hessian matrix
#' @param p P matrix from `h_get_kr_comp`
#' @param q Q matrix from `h_get_kr_comp`
#' @param r R matrix from `h_get_kr_comp`
#' @param order the order of the Kenward-Roger approximation. Only 1L or 2L supported.
#' @keywords internal
v_a <- function(v, w, p, q, r, order = 2L) {
  if (identical(order, 1L)) {
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
  return(ret)
}
