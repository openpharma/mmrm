test_that("contrast-space KR agrees with coefficient-space inference on fitted models", {
  complete_ids <- names(which(table(fev_data$USUBJID) == nlevels(fev_data$AVISIT)))
  complete_data <- droplevels(fev_data[fev_data$USUBJID %in% complete_ids, ])
  cases <- list(
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID), complete_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | SEX / USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + ar1(AVISIT | USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + sp_exp(VISITN, VISITN2 | SEX / USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + sp_gau(VISITN, VISITN2 | USUBJID), fev_data)
  )
  for (case in cases) {
    for (weighted in c(FALSE, TRUE)) {
      dat <- case[[2L]]
      weights <- if (weighted) seq(0.5, 2, length.out = nrow(dat)) else rep(1, nrow(dat))
      fit <- mmrm(case[[1L]], dat, weights = weights, method = "Kenward-Roger")
      v0 <- fit$beta_vcov
      w <- component(fit, "theta_vcov")
      p <- fit$kr_comp$P
      n_beta <- ncol(v0)
      # Dense, nonorthogonal contrasts exercise normalization and off-diagonal F entries.
      contrasts <- diag(n_beta) + matrix(seq_len(n_beta^2) / n_beta^3, n_beta)
      for (rank in c(1L, 2L, 3L, n_beta)) {
        contrast <- contrasts[seq_len(rank), , drop = FALSE]
        info <- paste(format(case[[1L]]), weighted, rank, nrow(dat))
        expected <- h_kr_df_coefficient_space(v0, contrast, w, p)
        actual <- h_kr_df(v0, contrast, w, p)
        expect_equal(actual, expected, tolerance = 1e-9, info = info)
        for (linear in c(FALSE, TRUE)) {
          reference <- fit
          if (linear) {
            reference$vcov <- "Kenward-Roger-Linear"
            reference$beta_vcov_adj <- h_var_adj(v0, w, p, fit$kr_comp$Q, NULL, linear = TRUE)
          }
          expect_equal(df_md(reference, contrast),
            h_test_md(reference, contrast, expected$m, expected$lambda),
            tolerance = 1e-9, info = info)
          if (rank == 1L) {
            scalar <- df_1d(reference, as.vector(contrast))
            expect_equal(scalar, h_test_1d(reference, as.vector(contrast), expected$m),
              tolerance = 1e-9, info = info)
            expect_equal(df_md(reference, contrast)$f_stat, scalar$t_stat^2, tolerance = 1e-12)
            expect_equal(df_md(reference, contrast)$p_val, scalar$p_val, tolerance = 1e-12)
          }
        }
      }
      # Invertible row transformations represent the same hypothesis.
      contrast <- contrasts[1:3, , drop = FALSE]
      transform <- matrix(c(2, 1, -1, 0, 3, 1, 1, 0, 2), 3)
      expect_equal(h_kr_df(v0, transform %*% contrast, w, p), h_kr_df(v0, contrast, w, p),
        tolerance = 1e-9)
      if (fit$tmb_data$n_groups > 1L) {
        group <- rep(seq_len(fit$tmb_data$n_groups), each = ncol(w) / fit$tmb_data$n_groups)
        block_w <- w
        block_w[outer(group, group, "!=")] <- 0
        expect_gt(max(abs(w - block_w)), 1e-8)
        expect_gt(abs(h_kr_df(v0, contrast, w, p)$m - h_kr_df(v0, contrast, block_w, p)$m), 1e-6)
      }
    }
  }
})

test_that("scalar shortcut handles one coefficient, rescaling and boundary moments", {
  for (moment in c(0, 0.01, 1, 2)) {
    expected <- list(m = 2 / moment, lambda = 1)
    for (scale in c(-3, 1, 1e-8, 1e8)) {
      expect_equal(h_kr_df(matrix(2), matrix(scale), matrix(moment / 64), matrix(4)), expected)
    }
  }
})

test_that("contrast-space calculation supports one covariance parameter and ill-conditioned covariance", {
  v0 <- diag(c(1e-6, 0.2, 1, 10, 100))
  v0[2, 3] <- v0[3, 2] <- 0.1
  p <- diag(1 / diag(v0))
  contrast <- matrix(c(1, 2, -1, 0, 1, 0, 1, 3, -2, 1), 2, byrow = TRUE)
  w <- matrix(0.001)
  expected <- h_kr_df_coefficient_space(v0, contrast, w, p)
  expect_equal(h_kr_df(v0, contrast, w, p), expected, tolerance = 1e-9)
  scaled <- diag(c(1e-3, 1e3)) %*% contrast
  expect_equal(h_kr_df(v0, scaled, w, p), expected, tolerance = 1e-9)
  expect_equal(h_kr_df(v0, diag(5), w, p),
    h_kr_df_coefficient_space(v0, diag(5), w, p), tolerance = 1e-9)
})

test_that("contrast-space calculation rejects incompatible components and singular hypotheses", {
  expect_error(h_kr_df(diag(2), diag(2), diag(2), matrix(0, 4, 3)), "2 cols")
  expect_error(h_kr_df(diag(2), diag(2), diag(2), matrix(0, 2, 2)), "4 rows")
  expect_error(h_kr_df(diag(2), matrix(1, 2, 2), matrix(1), diag(2)), "numerically singular")
})
