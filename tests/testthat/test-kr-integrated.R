# Compare the public KR-linear path to BOTH retained pairwise formulas.
# Reusing df_1d()/df_md() on a reference fit would reuse the new df algorithm.
expect_kr_linear_reference <- function(fit, tolerance = 1e-9) {
  w <- component(fit, "theta_vcov")
  # The full KR path still uses the original second-order derivative caches and
  # pairwise loop, so its P and Q are computed exactly as before the
  # optimization. R is not needed for the linear reference.
  legacy <- h_get_kr_comp(fit$tmb_data, fit$theta_est)
  covariance <- h_var_adj(fit$beta_vcov, w, legacy$P, legacy$Q, NULL, linear = TRUE)
  reference <- fit
  reference$beta_vcov_adj <- covariance
  p <- length(coef(fit))

  expect_null(fit$kr_comp$Q)
  expect_null(fit$kr_comp$R)
  expect_equal(fit$kr_comp$P, legacy$P, tolerance = tolerance)
  expect_equal(vcov(fit), covariance, tolerance = tolerance, ignore_attr = TRUE)
  expect_lt(max(abs(sqrt(diag(vcov(fit)) / diag(covariance)) - 1)), tolerance)
  # An absolute bound in standardized coordinates remains meaningful when
  # a poorly scaled design produces coefficient variances of very different sizes.
  scale <- sqrt(outer(diag(covariance), diag(covariance)))
  expect_lt(max(abs((fit$beta_vcov_adj - covariance) / scale)), tolerance)

  contrasts <- diag(p) + matrix(seq_len(p^2) / p^3, p)
  for (rank in unique(c(1L, min(2L, p), min(3L, p), p))) {
    contrast <- contrasts[seq_len(rank), , drop = FALSE]
    expected <- h_kr_df_coefficient_space(fit$beta_vcov, contrast, w, legacy$P)
    actual <- h_kr_df(fit$beta_vcov, contrast, w, fit$kr_comp$P)
    expect_equal(actual, expected, tolerance = tolerance)
    expect_true(is.finite(actual$m) && actual$m > 0)
    expect_true(is.finite(actual$lambda) && actual$lambda > 0)
    result <- df_md(fit, contrast)
    expected_result <- h_test_md(reference, contrast, expected$m, expected$lambda)
    expect_equal(result, expected_result, tolerance = tolerance)
    # Log tails catch changes to tiny p-values that absolute tolerances hide.
    expect_equal(log(result$p_val), log(expected_result$p_val), tolerance = tolerance)
    expect_true(is.finite(result$p_val) && result$p_val >= 0 && result$p_val <= 1)
    if (rank == 1L) {
      scalar <- df_1d(fit, as.vector(contrast))
      expect_equal(scalar, h_test_1d(reference, as.vector(contrast), expected$m),
        tolerance = tolerance)
      expect_equal(result$f_stat, scalar$t_stat^2, tolerance = tolerance)
      expect_equal(result$p_val, scalar$p_val, tolerance = tolerance)
    }
  }

  # Every coefficient's public summary includes its adjusted SE, scalar df,
  # t statistic, and p-value; this also exercises the integrated summary path.
  expected <- unname(t(vapply(seq_len(p), function(j) {
    contrast <- diag(p)[j, , drop = FALSE]
    df <- h_kr_df_coefficient_space(fit$beta_vcov, contrast, w, legacy$P)$m
    unlist(h_test_1d(reference, as.vector(contrast), df))
  }, numeric(5L))))
  actual <- unname(summary(fit)$coefficients)
  # Compare column by column, so that large df values cannot mask relative
  # differences in the much smaller standard errors or p-values.
  for (j in seq_len(ncol(expected))) {
    expect_equal(actual[, j], expected[, j], tolerance = tolerance)
  }
  expect_equal(log(actual[, 5L]), log(expected[, 5L]), tolerance = tolerance)
}

test_that("integrated KR-linear inference matches the old formulas across data patterns", {
  skip_on_cran()
  # fev_data already misses 263 of its 800 FEV1 values: of the 197 subjects in
  # the fit, 39 have all 4 visits and 21 only one. The two additional patterns
  # remove rows on top of this, keeping visits 1 to (subject %% 4 + 1), or all
  # visits but that one. The latter leaves no subject with all visits observed.
  subject <- as.integer(fev_data$USUBJID)
  visit <- as.integer(fev_data$AVISIT)
  patterns <- list(
    original = fev_data,
    monotone = droplevels(fev_data[visit <= subject %% 4L + 1L, ]),
    intermittent = droplevels(fev_data[visit != subject %% 4L + 1L, ])
  )
  expect_true(any(table(patterns$monotone$USUBJID) == 1L))
  expect_true(any(vapply(split(as.integer(patterns$intermittent$AVISIT),
    patterns$intermittent$USUBJID), function(visits) any(diff(sort(visits)) > 1L), logical(1L))))
  for (dat in patterns) {
    for (grouped in c(FALSE, TRUE)) {
      formula <- if (grouped) {
        FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | SEX / USUBJID)
      } else {
        FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID)
      }
      for (weighted in c(FALSE, TRUE)) {
        weights <- if (weighted) seq(0.2, 3, length.out = nrow(dat)) else rep(1, nrow(dat))
        fit <- mmrm(formula, dat, weights = weights,
          method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
        expect_kr_linear_reference(fit)
        # The covariance preparation must not change the fitted estimates.
        sat <- mmrm(formula, dat, weights = weights, method = "Satterthwaite")
        expect_equal(coef(fit), coef(sat), tolerance = 1e-12)
        expect_equal(fit$theta_est, sat$theta_est, tolerance = 1e-12)
        expect_equal(fit$beta_vcov, sat$beta_vcov, tolerance = 1e-12)
      }
    }
  }
})

test_that("integrated KR-linear supports a poorly conditioned fitted residual covariance", {
  skip_on_cran()
  dat <- fev_data
  dat$FEV1 <- dat$FEV1 * c(0.03, 1, 3, 30)[as.integer(dat$AVISIT)]
  fit <- mmrm(FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID), dat,
    method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
  covariance <- component(fit, "varcor")
  expect_equal(component(fit, "convergence"), 0L)
  expect_gt(kappa(covariance, exact = TRUE), 1e6)
  expect_gt(min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_gt(min(eigen(fit$beta_vcov, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_gt(min(eigen(component(fit, "theta_vcov"), symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_kr_linear_reference(fit, tolerance = 1e-8)
})

test_that("integrated KR-linear supports poorly conditioned but valid fitted designs", {
  skip_on_cran()
  dat <- fev_data
  dat$baseline_small <- 1e-4 * dat$FEV1_BL
  dat$baseline_close <- dat$FEV1_BL + 0.03 * sin(as.integer(dat$USUBJID))
  dat$baseline_extreme <- dat$FEV1_BL + 0.001 * sin(as.integer(dat$USUBJID))
  cases <- list(
    list(FEV1 ~ ARMCD * AVISIT + baseline_small + us(AVISIT | USUBJID), 1e-7, 1e6),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + baseline_close + us(AVISIT | SEX / USUBJID), 1e-7, 1e6),
    # At condition numbers near 1e10 both backends accumulate appreciable
    # roundoff. Keep this regression with an explicit 0.1% bound, rather than
    # claiming the ordinary 1e-9 equivalence tolerance applies to such a fit.
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + baseline_extreme + us(AVISIT | SEX / USUBJID), 1e-3, 1e9)
  )
  for (case in cases) {
    fit <- mmrm(case[[1L]], dat, weights = seq(0.5, 2, length.out = nrow(dat)),
      method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
    expect_equal(component(fit, "convergence"), 0L)
    expect_gt(kappa(fit$beta_vcov, exact = TRUE), case[[3L]])
    expect_gt(min(eigen(fit$beta_vcov, symmetric = TRUE, only.values = TRUE)$values), 0)
    expect_gt(min(eigen(component(fit, "theta_vcov"), symmetric = TRUE, only.values = TRUE)$values), 0)
    expect_kr_linear_reference(fit, tolerance = case[[2L]])
  }
})

test_that("integrated KR-linear handles a single visit and empty contrasts", {
  dat <- droplevels(fev_data[fev_data$AVISIT == levels(fev_data$AVISIT)[1L], ])
  fit <- mmrm(FEV1 ~ 1 + us(AVISIT | USUBJID), dat,
    method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
  expect_kr_linear_reference(fit)
  expect_equal(df_1d(fit, 0), list(est = 0, se = NA_real_, df = NA_real_, t_stat = NA_real_, p_val = NA_real_))
  expect_equal(df_md(fit, matrix(numeric(), 0L, 1L)),
    list(num_df = 0, denom_df = NA_real_, f_stat = NA_real_, p_val = NA_real_))
  expect_error(df_md(fit, matrix(1, 2L, 1L)), "numerically singular")
  ml <- fit
  ml$reml <- FALSE
  expect_error(df_1d(ml, 1), "only for REML")
  expect_error(df_md(ml, matrix(1)), "only for REML")
  expect_error(mmrm(FEV1 ~ 1 + us(AVISIT | USUBJID), dat, reml = FALSE,
    method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"), "only works for REML")
})
