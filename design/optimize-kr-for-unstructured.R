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

# Last visit of each of n subjects, uniformly from first:m. Unlike
# sample(first:m, ...), this does not sample from 1:m when first == m, and it
# gives the same random numbers otherwise.
sample_last_visit <- function(first, m, n) {
  stopifnot(first <= m)
  first - 1L + sample.int(m - first + 1L, n, replace = TRUE)
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
  last <- sample_last_visit(10L, m, n)
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

# Shared data for the small incremental fit and contrast benchmarks.
kr_steps_data <- function(n = 100L, m = 6L) {
  stopifnot(n %% 2L == 0L, m >= 2L)
  set.seed(20261001)
  dat <- expand.grid(visit = seq_len(m), id = seq_len(n))
  dat$id <- factor(dat$id)
  dat$trt <- factor(rep(rep(c("A", "B"), each = n / 2), each = m))
  dat$baseline <- rep(rnorm(n), each = m)
  sigma <- 0.5^abs(outer(seq_len(m), seq_len(m), "-"))
  e <- matrix(rnorm(n * m), n, m) %*% chol(sigma)
  dat$y <- 0.3 * dat$baseline + 0.2 * (dat$trt == "B") + as.vector(t(e))
  last <- sample_last_visit(ceiling(0.6 * m), m, n)
  dat <- dat[dat$visit <= rep(last, each = m), ]
  dat$visit <- factor(dat$visit)
  dat
}

# Small repeatable full-fit benchmark for tracking implementation steps.
# Run in separate R sessions with baseline and updated checkouts loaded using
# the same compiler flags. No prototype calculation or contrast timing.
benchmark_kr_steps <- function(n = 100L, m = 6L, repetitions = 3L) {
  stopifnot(repetitions >= 1L)
  dat <- kr_steps_data(n, m)
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

# Post-fit df benchmark: batch calls so that small elapsed times are measurable.
# Pass a saved implementation as df_fun to compare on exactly the same fit.
benchmark_kr_df_steps <- function(n = 100L, m = 6L, repetitions = 3L,
                                  calls = 100L, df_fun = mmrm:::h_kr_df) {
  stopifnot(repetitions >= 1L, calls >= 1L)
  dat <- kr_steps_data(n, m)
  fit <- mmrm::mmrm(y ~ (baseline + trt) * visit + us(visit | id), dat,
    control = mmrm::mmrm_control(method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"))
  v0 <- fit$beta_vcov
  w <- mmrm::component(fit, "theta_vcov")
  p <- fit$kr_comp$P
  n_beta <- ncol(v0)
  ranks <- c(1L, 3L)
  contrasts <- lapply(ranks, function(rank) diag(n_beta)[seq.int(n_beta - rank + 1L, n_beta), , drop = FALSE])
  results <- lapply(contrasts, function(contrast) df_fun(v0, contrast, w, p))
  times <- vapply(contrasts, function(contrast) {
    replicate(repetitions, {
      gc()
      system.time(for (i in seq_len(calls)) df_fun(v0, contrast, w, p))[["elapsed"]]
    })
  }, numeric(repetitions))
  times <- matrix(times, nrow = repetitions, dimnames = list(NULL, paste0("rank", ranks)))
  print(times)
  print(apply(times, 2L, median) / calls)
  invisible(list(times = times, seconds_per_call = apply(times, 2L, median) / calls,
    results = results, calls = calls, fit = fit))
}

# Integrated benchmark, usable unchanged in the baseline and optimized sessions.
# This reproduces benchmark_kr()'s ORIGINAL dropout (10:m), including at m = 15;
# kr_steps_data() instead starts dropout at ceiling(0.6 * m).
# Fit timings include optimization, Hessian, and KR covariance preparation.
# Summary and contrast timings are separate; no prototype work is timed here.
# The compact numerical snapshot can be compared across R sessions/builds.
benchmark_kr_integrated <- function(n = 300L, m = 18L, repetitions = 3L) {
  stopifnot(n %% 2L == 0L, m >= 10L, repetitions >= 1L)
  set.seed(20261001)
  dat <- expand.grid(visit = seq_len(m), id = seq_len(n))
  dat$id <- factor(dat$id)
  dat$trt <- factor(rep(rep(c("A", "B"), each = n / 2), each = m))
  dat$baseline <- rep(rnorm(n), each = m)
  sigma <- 0.5^abs(outer(seq_len(m), seq_len(m), "-"))
  e <- matrix(rnorm(n * m), n, m) %*% chol(sigma)
  dat$y <- 0.3 * dat$baseline + 0.2 * (dat$trt == "B") + as.vector(t(e))
  last <- sample_last_visit(10L, m, n)
  dat <- dat[dat$visit <= rep(last, each = m), ]
  dat$visit <- factor(dat$visit)
  formula <- y ~ (baseline + trt) * visit + us(visit | id)
  benchmark_kr_fit(formula, dat, repetitions = repetitions)
}

# Common timing/snapshot engine, also used for the small validation scenarios
# below. Run each implementation in its own R session.
benchmark_kr_fit <- function(formula, dat, weights = NULL, repetitions = 1L) {
  stopifnot(repetitions >= 1L)
  times <- matrix(NA_real_, repetitions, 2L,
    dimnames = list(NULL, c("Satterthwaite", "KR-linear")))
  cpu_times <- times
  for (method in colnames(times)) {
    control <- if (method == "Satterthwaite") {
      mmrm::mmrm_control(method = method)
    } else {
      mmrm::mmrm_control(method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
    }
    for (i in seq_len(repetitions)) {
      gc()
      timing <- system.time(fit <- mmrm::mmrm(formula, dat, weights = weights, control = control))
      times[i, method] <- timing[["elapsed"]]
      cpu_times[i, method] <- sum(timing[c("user.self", "sys.self")])
      cat(fit$tmb_data$n_subjects, "subjects,", fit$tmb_data$n_visits,
        "visits:", method, "run", i, times[i, method], "s\n")
    }
    if (method == "Satterthwaite") sat_beta <- coef(fit)
  }
  stopifnot(isTRUE(all.equal(sat_beta, coef(fit))))
  gc()
  summary_timing <- system.time(coefficient_table <- summary(fit)$coefficients)
  summary_time <- summary_timing[["elapsed"]]
  p <- length(coef(fit))
  # Dense nonorthogonal hypotheses, plus the scalar public t-test.
  contrasts <- diag(p) + matrix(seq_len(p^2) / p^3, p)
  ranks <- unique(c(1L, min(2L, p), min(3L, p), p))
  inference <- lapply(ranks, function(rank) {
    contrast <- contrasts[seq_len(rank), , drop = FALSE]
    gc()
    elapsed <- system.time(result <- mmrm::df_md(fit, contrast))[["elapsed"]]
    moments <- mmrm:::h_kr_df(fit$beta_vcov, contrast, mmrm::component(fit, "theta_vcov"), fit$kr_comp$P)
    list(rank = rank, elapsed = elapsed, result = result, moments = moments,
      se = sqrt(diag(contrast %*% fit$beta_vcov_adj %*% t(contrast))))
  })
  scalar <- mmrm::df_1d(fit, as.vector(contrasts[1L, ]))
  stopifnot(all(is.finite(coefficient_table)), all(is.finite(fit$beta_vcov_adj)),
    all(vapply(inference, function(x) all(is.finite(unlist(x$result))) &&
      x$result$denom_df > 0 && x$moments$lambda > 0, logical(1L))))
  print(times)
  print(apply(times, 2L, median))
  cat("Summary:", summary_time, "s\n")
  invisible(list(
    dimensions = c(subjects = fit$tmb_data$n_subjects, visits = fit$tmb_data$n_visits,
      observations = length(fit$tmb_data$y_vector), coefficients = p,
      covariance_parameters = length(fit$theta_est)),
    repetitions = repetitions,
    times = times, cpu_times = cpu_times, median_fit = apply(times, 2L, median),
    median_fit_cpu = apply(cpu_times, 2L, median), summary_time = summary_time,
    summary_cpu = sum(summary_timing[c("user.self", "sys.self")]),
    snapshot = list(beta = coef(fit), theta = fit$theta_est,
      unadjusted_covariance = fit$beta_vcov, covariance = fit$beta_vcov_adj,
      coefficients = coefficient_table, scalar = scalar,
      contrasts = lapply(inference, function(x) x[c("rank", "result", "moments", "se")])),
    contrast_times = setNames(vapply(inference, function(x) x$elapsed, numeric(1L)), ranks),
    session = sessionInfo()
  ))
}

# Small end-to-end comparisons across the same scenario matrix as the tests.
# The expensive 15/18-visit runs remain separate, so no long job runs on source().
benchmark_kr_cases <- function(repetitions = 1L) {
  # As in tests/testthat/test-kr-integrated.R: fev_data already misses 263 of
  # its 800 FEV1 values; the monotone and intermittent patterns remove further
  # rows on top of this.
  dat <- mmrm::fev_data
  subject <- as.integer(dat$USUBJID)
  visit <- as.integer(dat$AVISIT)
  patterns <- list(
    original = dat,
    monotone = droplevels(dat[visit <= subject %% 4L + 1L, ]),
    intermittent = droplevels(dat[visit != subject %% 4L + 1L, ])
  )
  results <- list()
  for (pattern in names(patterns)) for (grouped in c(FALSE, TRUE)) for (weighted in c(FALSE, TRUE)) {
    d <- patterns[[pattern]]
    formula <- if (grouped) {
      FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | SEX / USUBJID)
    } else {
      FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID)
    }
    weights <- if (weighted) seq(0.2, 3, length.out = nrow(d)) else rep(1, nrow(d))
    name <- paste(pattern, if (grouped) "grouped" else "ungrouped",
      if (weighted) "weighted" else "unweighted", sep = "_")
    cat("Scenario:", name, "\n")
    results[[name]] <- benchmark_kr_fit(formula, d, weights, repetitions)
  }
  dat$baseline_small <- 1e-4 * dat$FEV1_BL
  dat$baseline_close <- dat$FEV1_BL + 0.03 * sin(as.integer(dat$USUBJID))
  dat$baseline_extreme <- dat$FEV1_BL + 0.001 * sin(as.integer(dat$USUBJID))
  formulas <- list(
    scaled_design = FEV1 ~ ARMCD * AVISIT + baseline_small + us(AVISIT | USUBJID),
    collinear_design = FEV1 ~ ARMCD * AVISIT + FEV1_BL + baseline_close + us(AVISIT | SEX / USUBJID),
    extreme_design = FEV1 ~ ARMCD * AVISIT + FEV1_BL + baseline_extreme + us(AVISIT | SEX / USUBJID)
  )
  for (name in names(formulas)) {
    cat("Scenario:", name, "\n")
    results[[name]] <- benchmark_kr_fit(formulas[[name]], dat,
      seq(0.5, 2, length.out = nrow(dat)), repetitions)
  }
  dat$FEV1 <- dat$FEV1 * c(0.03, 1, 3, 30)[as.integer(dat$AVISIT)]
  results$residual_scales <- benchmark_kr_fit(
    FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID), dat, repetitions = repetitions)
  invisible(results)
}

# All 19 scenarios of the step-4 comparison: the three large examples, then the
# small cases. Run once per build, each in a fresh R session, and save the result.
benchmark_kr_all <- function(large_repetitions = 1L, small_repetitions = 1L) {
  c(
    list(
      "300-15" = benchmark_kr_integrated(300L, 15L, large_repetitions),
      "300-18" = benchmark_kr_integrated(300L, 18L, large_repetitions),
      "900-18" = benchmark_kr_integrated(900L, 18L, large_repetitions)
    ),
    benchmark_kr_cases(small_repetitions)
  )
}

# Compare completed benchmark snapshots, not prototype-only quantities.
# Relative comparisons are supplemented by an absolute covariance bound in
# standardized coordinates, which is also appropriate for poorly scaled fits.
check_kr_integrated <- function(before, after, tolerance = 1e-8) {
  stopifnot(identical(before$dimensions, after$dimensions))
  stopifnot(isTRUE(all.equal(before$snapshot, after$snapshot, tolerance = tolerance)))
  old <- before$snapshot$covariance
  new <- after$snapshot$covariance
  scaled_error <- max(abs((old - new) / sqrt(outer(diag(old), diag(old)))))
  stopifnot(scaled_error < tolerance)
  old_log_p <- c(log(before$snapshot$coefficients[, 5L]),
    vapply(before$snapshot$contrasts, function(x) log(x$result$p_val), numeric(1L)))
  new_log_p <- c(log(after$snapshot$coefficients[, 5L]),
    vapply(after$snapshot$contrasts, function(x) log(x$result$p_val), numeric(1L)))
  stopifnot(isTRUE(all.equal(old_log_p, new_log_p, tolerance = tolerance)))
  moment_error <- function(name) {
    max(vapply(seq_along(before$snapshot$contrasts), function(i) {
      abs(before$snapshot$contrasts[[i]]$moments[[name]] - after$snapshot$contrasts[[i]]$moments[[name]])
    }, numeric(1L)))
  }
  # Contrast timings by rank; the full rank is the number of coefficients.
  p <- before$dimensions[["coefficients"]]
  rank_names <- c(rank1 = "1", rank2 = "2", rank3 = "3", fullrank = as.character(p))
  rank_times <- function(result) {
    setNames(as.list(unname(result$contrast_times[rank_names])), names(rank_names))
  }
  before_fit <- before$median_fit[["KR-linear"]]
  after_fit <- after$median_fit[["KR-linear"]]
  data.frame(
    as.list(before$dimensions),
    tolerance = tolerance,
    before_seconds = before_fit,
    after_seconds = after_fit,
    speedup = before_fit / after_fit,
    before_satterthwaite_seconds = before$median_fit[["Satterthwaite"]],
    after_satterthwaite_seconds = after$median_fit[["Satterthwaite"]],
    before_fit_cpu_seconds = before$median_fit_cpu[["KR-linear"]],
    after_fit_cpu_seconds = after$median_fit_cpu[["KR-linear"]],
    before_repetitions = before$repetitions,
    after_repetitions = after$repetitions,
    before_summary_seconds = before$summary_time,
    after_summary_seconds = after$summary_time,
    before_fit_and_summary = before_fit + before$summary_time,
    after_fit_and_summary = after_fit + after$summary_time,
    fit_and_summary_speedup = (before_fit + before$summary_time) / (after_fit + after$summary_time),
    before = rank_times(before),
    after = rank_times(after),
    max_covariance_error = max(abs(old - new)),
    standardized_covariance_error = scaled_error,
    max_relative_se_error = max(abs(sqrt(diag(new) / diag(old)) - 1)),
    max_df_error = moment_error("m"),
    max_scale_error = moment_error("lambda"),
    max_coefficient_table_error = max(abs(before$snapshot$coefficients - after$snapshot$coefficients)),
    max_log_p_error = max(abs(old_log_p[is.finite(old_log_p)] - new_log_p[is.finite(old_log_p)]))
  )
}

# Comparison tolerances that differ from the default 1e-8, matching
# tests/testthat/test-kr-integrated.R and the documented numerical limits.
kr_integrated_tolerances <- c(collinear_design = 1e-7, extreme_design = 1e-3)

# Combine saved benchmark_kr_all() results of both builds into the recorded
# table, optionally writing it to a CSV file.
kr_integrated_table <- function(before, after, file = NULL) {
  stopifnot(identical(names(before), names(after)))
  rows <- lapply(names(before), function(name) {
    tolerance <- if (name %in% names(kr_integrated_tolerances)) kr_integrated_tolerances[[name]] else 1e-8
    cbind(scenario = name, check_kr_integrated(before[[name]], after[[name]], tolerance))
  })
  table <- do.call(rbind, rows)
  if (!is.null(file)) {
    utils::write.csv(table, file, row.names = FALSE)
  }
  table
}
