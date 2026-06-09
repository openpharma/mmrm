# mmrm_control() emmeans_gcomp_vars argument ----

test_that("mmrm accepts emmeans_gcomp_vars", {
  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    control = mmrm_control(emmeans_gcomp_vars = c("ARMCD", "AVISIT"))
  )
  expect_identical(fit$tmb_data$emmeans_gcomp_vars, c("ARMCD", "AVISIT"))
})

test_that("mmrm without emmeans_gcomp_vars has NULL fields", {
  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  expect_null(fit$tmb_data$emmeans_gcomp_vars)
})

test_that("mmrm errors if emmeans_gcomp_vars variable not in data", {
  expect_error(
    mmrm(
      FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
      data = fev_data,
      control = mmrm_control(emmeans_gcomp_vars = c("NONEXISTENT", "AVISIT"))
    ),
    "subset"
  )
})

test_that("emmeans_gcomp_vars does not change model fit", {
  fit_with <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    control = mmrm_control(emmeans_gcomp_vars = c("ARMCD", "AVISIT"))
  )
  fit_without <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  expect_equal(component(fit_with, "beta_est"), component(fit_without, "beta_est"))
  expect_equal(component(fit_with, "beta_vcov"), component(fit_without, "beta_vcov"))
})

# h_get_subject_data ----

test_that("h_get_subject_data returns one row per subject", {
  fit <- get_mmrm_gcomp()
  subj_data <- h_get_subject_data(fit)
  expect_equal(nrow(subj_data), length(unique(fev_data$USUBJID)))
  expect_true("USUBJID" %in% names(subj_data))
})

test_that("h_get_subject_data uses original data including subjects with missing outcomes", {
  fit <- get_mmrm_gcomp()
  subj_data <- h_get_subject_data(fit)
  n_fit_subjects <- component(fit, "n_subjects")
  # Original data may have more subjects than the fit (due to NA removal).
  expect_true(nrow(subj_data) >= n_fit_subjects)
})

# h_compute_potential_outcomes ----

test_that("h_compute_potential_outcomes returns correct dimensions", {
  fit <- get_mmrm_gcomp()
  subj_data <- h_get_subject_data(fit)
  trt_levels <- levels(fev_data$ARMCD)
  cf_grid <- data.frame(ARMCD = factor(trt_levels, levels = trt_levels))

  po <- h_compute_potential_outcomes(fit, subj_data, "VIS4", "AVISIT", cf_grid)
  expect_matrix(po$vhat, nrows = nrow(subj_data), ncols = length(trt_levels))
  expect_matrix(po$L_global, nrows = length(trt_levels), ncols = length(component(fit, "beta_est")))
  expect_true(all(is.finite(po$vhat)))
})

test_that("potential outcomes differ between treatments", {
  fit <- get_mmrm_gcomp()
  subj_data <- h_get_subject_data(fit)
  trt_levels <- levels(fev_data$ARMCD)
  cf_grid <- data.frame(ARMCD = factor(trt_levels, levels = trt_levels))

  po <- h_compute_potential_outcomes(fit, subj_data, "VIS4", "AVISIT", cf_grid)
  expect_false(abs(mean(po$vhat[, 1]) - mean(po$vhat[, 2])) < 1e-10)
})

# h_gcomp_emm_correction ----

test_that("h_gcomp_emm_correction returns PSD delta and L_global", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_gcomp()
  rg <- emmeans::ref_grid(fit)
  correction <- h_gcomp_emm_correction(fit, rg@linfct, rg@grid)

  p <- length(component(fit, "beta_est"))
  expect_matrix(correction$delta, nrows = p, ncols = p)
  expect_true(isSymmetric(correction$delta, tol = sqrt(.Machine$double.eps)))

  eigen_vals <- eigen(correction$delta, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigen_vals >= -sqrt(.Machine$double.eps)))

  expect_matrix(correction$L_global, nrows = nrow(rg@grid), ncols = p)
})

test_that("h_gcomp_emm_correction trims wider model_mat from h_match_coefs expansion", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_gcomp()
  rg <- emmeans::ref_grid(fit)

  # Simulate h_match_coefs expansion: add an extra column for an aliased coef.
  linfct <- rg@linfct
  extra <- matrix(0, nrow = nrow(linfct), ncol = 1)
  colnames(extra) <- "ALIASED:COEF"
  model_mat_wide <- cbind(linfct, extra)

  correction <- h_gcomp_emm_correction(fit, model_mat_wide, rg@grid)

  p <- length(component(fit, "beta_est"))
  expect_matrix(correction$delta, nrows = p, ncols = p)
  expect_matrix(correction$L_global, nrows = nrow(rg@grid), ncols = p)
})

test_that("h_gcomp_emm_correction uses pseudoinverse when LLt is singular", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  # Additive model: p < K, so L_global has rank < K and LLt is singular.
  # K = 8 (2 arms x 4 visits), p = 5 (intercept + FEV1_BL + ARMCD + 3 AVISIT).
  fit <- get_mmrm_gcomp_additive()
  rg <- emmeans::ref_grid(fit)
  correction <- h_gcomp_emm_correction(fit, rg@linfct, rg@grid)

  p <- length(component(fit, "beta_est"))
  expect_matrix(correction$delta, nrows = p, ncols = p)
  expect_true(isSymmetric(correction$delta, tol = sqrt(.Machine$double.eps)))

  eigen_vals <- eigen(correction$delta, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigen_vals >= -sqrt(.Machine$double.eps)))
})

# emmeans integration ----

test_that("emmeans with gcomp outputs message and produces larger SEs than without", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit_corr <- get_mmrm_gcomp()
  fit_std <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )

  expect_message(
    emm_corr <- emmeans::emmeans(fit_corr, ~ ARMCD | AVISIT),
    "G-computation correction applied"
  )
  emm_corr <- as.data.frame(emm_corr)
  emm_std <- as.data.frame(expect_silent(emmeans::emmeans(fit_std, ~ ARMCD | AVISIT)))

  expect_true(all(emm_corr$SE >= emm_std$SE - sqrt(.Machine$double.eps)))
})

test_that("gcomp correction matches reference values on fev_data", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_gcomp()
  emm <- as.data.frame(suppressMessages(
    emmeans::emmeans(fit, ~ ARMCD | AVISIT)
  ))
  # Reference values from independent validation.
  expected_emmeans <- c(
    32.618448, 37.283436, 37.47562, 41.843852,
    42.950618, 46.517238, 47.989686, 53.001744
  )
  expected_ses <- c(
    0.773949, 0.787878, 0.613805, 0.609102,
    0.524801, 0.577069, 1.210581, 1.213523
  )
  expect_equal(emm$emmean, expected_emmeans, tolerance = 1e-4)
  expect_equal(emm$SE, expected_ses, tolerance = 1e-4)
})

test_that("additive model contrasts are unaffected by gcomp correction", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit_corr <- get_mmrm_gcomp_additive()
  fit_std <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD + AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )

  pairs_corr <- as.data.frame(pairs(suppressMessages(
    emmeans::emmeans(fit_corr, ~ ARMCD | AVISIT)
  )))
  pairs_std <- as.data.frame(pairs(expect_silent(
    emmeans::emmeans(fit_std, ~ ARMCD | AVISIT)
  )))

  ratio <- pairs_corr$SE / pairs_std$SE
  expect_true(all(ratio >= 0.99 & ratio <= 1.01))
})

# Compatibility with vcov methods ----

test_that("gcomp correction works with Kenward-Roger", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    control = mmrm_control(
      method = "Kenward-Roger",
      emmeans_gcomp_vars = c("ARMCD", "AVISIT")
    )
  )
  emm <- as.data.frame(suppressMessages(emmeans::emmeans(fit, ~ ARMCD | AVISIT)))
  expect_true(all(is.finite(emm$SE)))
  expect_true(all(emm$SE > 0))
  expect_true(all(is.finite(emm$df)))
})

test_that("gcomp correction works with Empirical vcov", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    control = mmrm_control(
      vcov = "Empirical",
      method = "Residual",
      emmeans_gcomp_vars = c("ARMCD", "AVISIT")
    )
  )
  emm <- as.data.frame(suppressMessages(emmeans::emmeans(fit, ~ ARMCD | AVISIT)))
  expect_true(all(is.finite(emm$SE)))
  expect_true(all(emm$SE > 0))
})

# Aliased coefficients ----

test_that("gcomp correction works with singular design matrix", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  dat <- fev_data
  dat$ARMCD2 <- dat$ARMCD

  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + ARMCD2 + us(AVISIT | USUBJID),
    data = dat,
    control = mmrm_control(
      accept_singular = TRUE,
      emmeans_gcomp_vars = c("ARMCD", "AVISIT")
    )
  )
  # Aliased models emit "Results may be misleading" messages from emmeans.
  emm <- as.data.frame(suppressMessages(
    emmeans::emmeans(fit, ~ ARMCD | AVISIT)
  ))
  expect_true(all(is.finite(emm$SE)))
  expect_true(all(emm$SE > 0))
})
