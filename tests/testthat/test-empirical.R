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

# unweighted mmrm ----

test_that("empirical covariance are the same with SAS result for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    vcov = "Empirical", method = "Residual"
  )
  expected <- 0.82581291651503
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.67973236317634
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.79354667955483
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.67851405488729
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toep", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.87390988854018
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toeph", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.68088497850656
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.64461343431037
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for us", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.63959860090712
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for sp_exp", {
  fit <- mmrm(FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- 0.80974821922674
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

# weighted mmrm ----

test_that("empirical covariance are the same with SAS result for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.9766083014759
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.77913503559498
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.90291936438319
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.76200584549261
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toep", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 1.00281379688484
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toeph", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.77628714727105
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.73422273273261
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for us", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.73089269734199
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for sp_exp", {
  fit <- mmrm(FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- 0.93828496200941
  expect_equal(sqrt(component(fit, "beta_vcov")[2, 2]), expected, tolerance = 1e-4)
})

## jackknife ----

test_that("Jackknife works as expected for ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(formula = formula, data = data_full, vcov = "Empirical-Jackknife", method = "Residual")
  fit_gls <- nlme::gls(FEV1 ~ ARMCD, data_full, correlation = nlme::corAR1(form = ~ VISITN | USUBJID))
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR3")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("Jackknife works as expected for weighted ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(
    formula = formula, data = data_full, vcov = "Empirical-Jackknife",
    method = "Residual", weights = data_full$WEIGHT
  )
  # the weights are different in gls and mmrm/SAS;
  data_full$WEIGHT2 <- 1 / data_full$WEIGHT
  fit_gls <- nlme::gls(
    FEV1 ~ ARMCD, data_full, correlation = nlme::corAR1(form = ~ VISITN | USUBJID),
    weights = nlme::varFixed(~ WEIGHT2)
  )
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR3")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-4, ignore_attr = TRUE)
})
