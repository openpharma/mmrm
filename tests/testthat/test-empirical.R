# h_get_empirical ----

test_that("h_get_empirical obtain empirical covariance", {
  fit <- get_mmrm_emp()
  expect_snapshot_tolerance(h_get_empirical(fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov, FALSE))
})

test_that("h_get_empirical obtain jackknife covariance", {
  fit <- get_mmrm_jack()
  expect_snapshot_tolerance(h_get_empirical(fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov, TRUE))
})

# integration test ----

test_that("empirical covariance are the same with SAS result for ar1", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, weights = fev_data$WEIGHT, cov = "Empirical", method = "Residual")
  expected <- 0.82581291651503
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.67973236317634
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.79354667955483
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.67851405488729
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toep", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.87390988854018
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toeph", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.68088497850656
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.64461343431037
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for us", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.63959860090712
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for sp_exp", {
  fit <- mmrm(FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID), data = fev_data, cov = "Empirical", method = "Residual")
  expected <- 0.80974821922674
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

# jackknife ----

test_that("Jackknife works as expected", {
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  data_full$USUBJID <- droplevels(data_full$USUBJID)
  ids <- lapply(levels(data_full$USUBJID), function(x) {
    which(data_full$USUBJID == x)
  })
  betas <- lapply(ids, function(i) {
    fit <- mmrm(formula = formula, data = data_full[-i, ])
    fit$beta_est
  })
  beta_all <- do.call(cbind, betas)
  fit <- mmrm(formula = formula, data = data_full, cov = "Empirical-Jackknife", method = "Residual")
  n <- component(fit, "n_subjects")
  expected <- (n - 1) / n * (beta_all - fit$beta_est) %*% t(beta_all - fit$beta_est)
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-2)
})
