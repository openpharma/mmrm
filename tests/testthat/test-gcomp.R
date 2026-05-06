# mmrm() gcomp_fixed_vars argument ----

test_that("mmrm accepts gcomp_fixed_vars", {
  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    gcomp_fixed_vars = c("ARMCD", "AVISIT")
  )
  expect_identical(fit$gcomp_fixed_vars, c("ARMCD", "AVISIT"))
  expect_data_frame(fit$gcomp_subject_data)
})

test_that("mmrm without gcomp_fixed_vars has NULL fields", {
  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  expect_null(fit$gcomp_fixed_vars)
  expect_null(fit$gcomp_subject_data)
})

test_that("mmrm errors if gcomp_fixed_vars variable not in data", {
  expect_error(
    mmrm(
      FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
      data = fev_data,
      gcomp_fixed_vars = c("NONEXISTENT", "AVISIT")
    ),
    "subset"
  )
})

test_that("gcomp_fixed_vars does not change model fit", {
  fit_with <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    gcomp_fixed_vars = c("ARMCD", "AVISIT")
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

# emmeans integration ----

test_that("emmeans with gcomp produces larger SEs than without", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit_corr <- get_mmrm_gcomp()
  fit_std <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )

  emm_corr <- as.data.frame(expect_silent(emmeans::emmeans(fit_corr, ~ ARMCD | AVISIT)))
  emm_std <- as.data.frame(expect_silent(emmeans::emmeans(fit_std, ~ ARMCD | AVISIT)))

  expect_true(all(emm_corr$SE >= emm_std$SE - sqrt(.Machine$double.eps)))
})

test_that("additive model contrasts are unaffected by gcomp correction", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit_corr <- get_mmrm_gcomp_additive()
  fit_std <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD + AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )

  pairs_corr <- as.data.frame(pairs(expect_silent(emmeans::emmeans(fit_corr, ~ ARMCD | AVISIT))))
  pairs_std <- as.data.frame(pairs(expect_silent(emmeans::emmeans(fit_std, ~ ARMCD | AVISIT))))

  ratio <- pairs_corr$SE / pairs_std$SE
  expect_true(all(ratio >= 0.99 & ratio <= 1.01))
})

# Compatibility with vcov methods ----

test_that("gcomp correction works with Kenward-Roger", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    gcomp_fixed_vars = c("ARMCD", "AVISIT")
  )
  emm <- as.data.frame(expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT)))
  expect_true(all(is.finite(emm$SE)))
  expect_true(all(emm$SE > 0))
  expect_true(all(is.finite(emm$df)))
})

test_that("gcomp correction works with Empirical vcov", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    FEV1 ~ FEV1_BL + ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    vcov = "Empirical",
    method = "Residual",
    gcomp_fixed_vars = c("ARMCD", "AVISIT")
  )
  emm <- as.data.frame(expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT)))
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
    control = mmrm_control(accept_singular = TRUE),
    gcomp_fixed_vars = c("ARMCD", "AVISIT")
  )
  emm <- as.data.frame(expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT)))
  expect_true(all(is.finite(emm$SE)))
  expect_true(all(emm$SE > 0))
})
