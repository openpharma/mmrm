# Frozen coefficient-space implementation of h_kr_df(): independent regression oracle.
h_kr_df_coefficient_space <- function(v0, l, w, p) {
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
  for (i in seq_along(pl)) {
    for (j in seq_along(pl)) {
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

# Explicit group/parameter-pair contraction, independent of the new directions.
h_kr_q_sum <- function(q, w, n_groups) {
  p <- ncol(q)
  k <- ncol(w) / n_groups
  result <- matrix(0, p, p)
  for (g in seq_len(n_groups)) {
    for (h in seq_len(k)) {
      for (j in seq_len(k)) {
        rows <- ((g - 1L) * k^2 + (h - 1L) * k + j - 1L) * p + seq_len(p)
        result <- result + w[(g - 1L) * k + h, (g - 1L) * k + j] * q[rows, , drop = FALSE]
      }
    }
  }
  result
}
