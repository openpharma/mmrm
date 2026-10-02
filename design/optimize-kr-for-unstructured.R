# Research prototype, not a package backend. One unstructured covariance group;
# at least two visits; arbitrary known positive weights and missing visits.
# source() defines helpers without fitting models. See the accompanying QMD.
kr_linear_contracted <- function(fit) {
  d <- fit$tmb_data
  stopifnot(d$cov_type == 'us', d$n_groups == 1L)
  m <- d$n_visits
  stopifnot(m >= 2L)
  p <- ncol(d$x_matrix)
  theta <- fit$theta_est
  k <- length(theta)
  L <- diag(exp(theta[seq_len(m)]))
  h <- m
  for (i in 2:m) for (j in seq_len(i - 1L)) {
    h <- h + 1L
    L[i, j] <- L[i, i] * theta[h]
  }
  Sigma <- tcrossprod(L)
  ij <- which(lower.tri(Sigma, diag = TRUE), arr.ind = TRUE)
  index <- matrix(0L, m, m)
  index[ij] <- seq_len(k)
  for (a in seq_len(k)) index[ij[a, 2], ij[a, 1]] <- a
  J <- matrix(0, k, k)
  for (h in seq_len(k)) {
    D <- matrix(0, m, m)
    if (h <= m) D[h, ] <- L[h, ] else {
      count <- m
      for (i in 2:m) for (j in seq_len(i - 1L)) {
        count <- count + 1L
        if (count == h) D[i, j] <- L[i, i]
      }
    }
    E <- tcrossprod(D, L) + tcrossprod(L, D)
    J[, h] <- E[ij]
  }
  W <- J %*% mmrm::component(fit, 'theta_vcov') %*% t(J)
  P <- matrix(0, p * p, k)
  QW <- matrix(0, p, p)
  # T[(u,v),(s,t)] = Cov(Sigma[u,s], Sigma[t,v]).
  uv <- expand.grid(u = seq_len(m), v = seq_len(m))
  T <- vapply(seq_len(m * m), function(b) {
    s <- uv$u[b]; t <- uv$v[b]
    W[cbind(index[cbind(uv$u, s)], index[cbind(t, uv$v)])]
  }, numeric(m * m))
  for (i in seq_len(d$n_subjects)) {
    rows <- d$subject_zero_inds[i] + seq_len(d$subject_n_visits[i])
    visits <- as.integer(d$coordinates[rows, 1]) + 1L
    A <- solve(Sigma[visits, visits, drop = FALSE])
    X <- sqrt(d$weights_vector[rows]) * d$x_matrix[rows, , drop = FALSE]
    U <- matrix(0, m, p)
    U[visits, ] <- A %*% X
    A_full <- matrix(0, m, m)
    A_full[visits, visits] <- A
    K <- matrix(T %*% as.vector(A_full), m, m)
    QW <- QW + crossprod(U, K %*% U)
    for (a in seq_len(k)) {
      u <- ij[a, 1]; v <- ij[a, 2]
      Pa <- -tcrossprod(U[u, ], U[v, ])
      if (u != v) Pa <- Pa + t(Pa)
      P[, a] <- P[, a] + as.vector(Pa)
    }
  }
  V <- fit$beta_vcov
  PW <- P %*% W
  PVPW <- matrix(0, p, p)
  for (a in seq_len(k)) PVPW <- PVPW + matrix(P[, a], p, p) %*% V %*% matrix(PW[, a], p, p)
  list(vcov = V + 2 * V %*% (QW - PVPW) %*% V,
       P = P, W = W, J = J, QW = QW)
}

kr_df_contracted <- function(V, contrast, W, P) {
  p <- ncol(V)
  c <- nrow(contrast)
  B <- forwardsolve(t(chol(contrast %*% V %*% t(contrast))), contrast %*% V)
  D <- vapply(seq_len(ncol(W)), function(h) {
    as.vector(B %*% matrix(P[, h], p, p) %*% t(B))
  }, numeric(c * c))
  D <- matrix(D, nrow = c * c)
  tr <- colSums(D[seq.int(1L, c * c, by = c + 1L), , drop = FALSE])
  a1 <- as.numeric(crossprod(tr, W %*% tr))
  a2 <- sum((D %*% W) * D)
  if (c == 1L) return(list(m = 2 / a2, lambda = 1))
  b <- (a1 + 6 * a2) / (2 * c)
  g <- ((c + 1) * a1 - (c + 4) * a2) / ((c + 2) * a2)
  denom <- 3 * c + 2 - 2 * g
  c1 <- g / denom; c2 <- (c - g) / denom; c3 <- (c + 2 - g) / denom
  e_star <- 1 / (1 - a2 / c)
  v_star <- 2 / c * (1 + c1 * b) / (1 - c2 * b)^2 / (1 - c3 * b)
  rho <- v_star / (2 * e_star^2)
  df <- 4 + (c + 2) / (c * rho - 1)
  list(m = df, lambda = df / (e_star * (df - 2)))
}

# Reproduce the numerical checks without running a long benchmark.
check_kr_contractions <- function() {
  results <- list()
  for (weighted in c(FALSE, TRUE)) {
    dat <- mmrm::fev_data
    set.seed(42)
    dat$w <- if (weighted) runif(nrow(dat), 0.5, 2) else 1
    fit <- mmrm::mmrm(
      FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID),
      dat, weights = dat$w,
      control = mmrm::mmrm_control(method = 'Kenward-Roger', vcov = 'Kenward-Roger-Linear')
    )
    a <- kr_linear_contracted(fit)
    p <- ncol(fit$beta_vcov)
    for (rank in c(1L, 3L)) {
      contrast <- diag(p)[seq_len(rank), , drop = FALSE]
      old <- mmrm:::h_kr_df(fit$beta_vcov, contrast,
                          mmrm::component(fit, 'theta_vcov'), fit$kr_comp$P)
      new <- kr_df_contracted(fit$beta_vcov, contrast, a$W, a$P)
      stopifnot(
        isTRUE(all.equal(a$vcov, fit$beta_vcov_adj, tolerance = 1e-10)),
        isTRUE(all.equal(old, new, tolerance = 1e-9))
      )
      results[[length(results) + 1L]] <- data.frame(
        weighted = weighted, rank = rank,
        max_cov_error = max(abs(a$vcov - fit$beta_vcov_adj)),
        df_error = abs(old$m - new$m), scale_error = abs(old$lambda - new$lambda)
      )
    }
  }
  do.call(rbind, results)
}

# End-to-end benchmark. Run explicitly; sourcing this file only defines helpers.
benchmark_kr <- function(n = 300L, m = 18L) {
  stopifnot(n %% 2L == 0L, m >= 10L)
  set.seed(20261001)
  dat <- expand.grid(visit = seq_len(m), id = seq_len(n))
  dat$id <- factor(dat$id)
  dat$trt <- factor(rep(rep(c('A', 'B'), each = n / 2), each = m))
  dat$baseline <- rep(rnorm(n), each = m)
  sigma <- 0.5^abs(outer(seq_len(m), seq_len(m), '-'))
  e <- matrix(rnorm(n * m), n, m) %*% chol(sigma)
  dat$y <- 0.3 * dat$baseline + 0.2 * (dat$trt == 'B') + as.vector(t(e))
  last <- sample(10:m, n, replace = TRUE)
  dat <- dat[dat$visit <= rep(last, each = m), ]
  dat$visit <- factor(dat$visit)
  f <- y ~ (baseline + trt) * visit + us(visit | id)
  sat_time <- system.time(sat <- mmrm::mmrm(
    f, dat, control = mmrm::mmrm_control(method = 'Satterthwaite')
  ))
  kr_time <- system.time(kr <- mmrm::mmrm(
    f, dat, control = mmrm::mmrm_control(
      method = 'Kenward-Roger', vcov = 'Kenward-Roger-Linear'
    )
  ))
  prototype_time <- system.time(a <- kr_linear_contracted(kr))
  stopifnot(
    isTRUE(all.equal(coef(sat), coef(kr))),
    isTRUE(all.equal(a$vcov, kr$beta_vcov_adj, tolerance = 1e-9))
  )
  timings <- data.frame(
    calculation = c('Satterthwaite fit', 'KR-linear fit', 'Prototype post-fit only'),
    elapsed = c(sat_time[['elapsed']], kr_time[['elapsed']], prototype_time[['elapsed']])
  )
  print(timings)
  invisible(list(timings = timings, sat = sat, kr = kr, prototype = a))
}

# Small repeatable full-fit benchmark for tracking implementation steps.
# Run in separate R sessions with baseline and updated checkouts loaded using
# the same compiler flags. No prototype calculation or contrast timing.
benchmark_kr_steps <- function(n = 100L, m = 6L, repetitions = 3L) {
  stopifnot(n %% 2L == 0L, m >= 2L, repetitions >= 1L)
  set.seed(20261001)
  dat <- expand.grid(visit = seq_len(m), id = seq_len(n))
  dat$id <- factor(dat$id)
  dat$trt <- factor(rep(rep(c("A", "B"), each = n / 2), each = m))
  dat$baseline <- rep(rnorm(n), each = m)
  sigma <- 0.5^abs(outer(seq_len(m), seq_len(m), "-"))
  e <- matrix(rnorm(n * m), n, m) %*% chol(sigma)
  dat$y <- 0.3 * dat$baseline + 0.2 * (dat$trt == "B") + as.vector(t(e))
  last <- sample(seq.int(ceiling(0.6 * m), m), n, replace = TRUE)
  dat <- dat[dat$visit <= rep(last, each = m), ]
  dat$visit <- factor(dat$visit)
  f <- y ~ (baseline + trt) * visit + us(visit | id)
  times <- sapply(c("Satterthwaite", "KR-linear"), function(method) {
    control <- if (method == "Satterthwaite") {
      mmrm::mmrm_control(method = method)
    } else {
      mmrm::mmrm_control(method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
    }
    replicate(repetitions, {
      gc()
      system.time(mmrm::mmrm(f, dat, control = control))[["elapsed"]]
    })
  })
  times <- matrix(times, nrow = repetitions,
    dimnames = list(NULL, c("Satterthwaite", "KR-linear")))
  print(times)
  print(apply(times, 2L, median))
  invisible(times)
}
