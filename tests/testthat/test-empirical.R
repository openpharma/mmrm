# h_get_empirical ----

test_that("h_get_empirical obtain empirical covariance", {
  fit <- get_mmrm_emp()
  result <- h_get_empirical(fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov, "Empirical")
  expect_snapshot_tolerance(result$cov)
})

test_that("h_get_empirical obtain jackknife covariance", {
  fit <- get_mmrm_jack()
  result <- h_get_empirical(fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov, "Empirical-Jackknife")
  expect_snapshot_tolerance(result$cov)
})

test_that("h_get_empirical obtain jackknife covariance", {
  fit <- get_mmrm_brl()
  result <- h_get_empirical(fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov, "Empirical-Bias-Reduced")
  expect_snapshot_tolerance(result$cov)
})

# integration test ----

# unweighted mmrm ----

test_that("empirical covariance are the same with SAS result for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    vcov = "Empirical", method = "Residual"
  )
  expected <- matrix(
    c(0.27666930670945, -0.27666930670945, -0.27666930670945, 0.68196697308307),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.19106397930347, -0.19106397930347, -0.19106397930347, 0.46203608554929),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.26910953746582, -0.26910953746582, -0.26910953746582, 0.6297163326325),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.19635239056617, -0.19635239056617, -0.19635239056617, 0.46038132267959),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toep", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.30022945455349, -0.30022945455349, -0.30022945455349, 0.76371849328831),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toeph", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.18350201218893, -0.18350201218893, -0.18350201218893, 0.46360435395588),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.17350223093416, -0.17350223093416, -0.17350223093416, 0.41552647969341),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for us", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data, vcov = "Empirical", method = "Residual")
  expected <- matrix(
    c(0.16725909903518, -0.16725909903518, -0.16725909903518, 0.40908637028234),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for sp_exp", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual"
  )
  expected <- matrix(
    c(0.26947559034166, -0.26947559034166, -0.26947559034166, 0.65569217854087),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

# weighted mmrm ----

test_that("empirical covariance are the same with SAS result for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.3741856039623, -0.3741856039623, -0.3741856039623, 0.95376377451164),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.22611242154248, -0.22611242154248, -0.22611242154248, 0.60705140369159),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.32402882795093, -0.32402882795093, -0.32402882795093, 0.81526337857816),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.21754684649458, -0.21754684649458, -0.21754684649458, 0.5806529085649),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toep", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.39272956491025, -0.39272956491025, -0.39272956491025, 1.00563551122259),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for toeph", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.22472893956932, -0.22472893956932, -0.22472893956932, 0.60262173501823),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.19660064483548, -0.19660064483548, -0.19660064483548, 0.53908302126134),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for us", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.198001710472, -0.198001710472, -0.198001710472, 0.53420413502786),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

test_that("empirical covariance are the same with SAS result for sp_exp", {
  fit <- mmrm(FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data, vcov = "Empirical", method = "Residual",
    weights = fev_data$WEIGHT
  )
  expected <- matrix(
    c(0.34876862866685, -0.34876862866685, -0.34876862866685, 0.88037866993301),
    ncol = 2,
    dimnames = rep(list(c("(Intercept)", "ARMCDTRT")), 2)
  )
  expect_equal(component(fit, "beta_vcov"), expected, tolerance = 1e-4)
})

## Empirical Satterthwaite vs gls/clubSandwich ----

test_that("Empirical works as expected for ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(formula = formula, data = data_full, vcov = "Empirical", method = "Satterthwaite")
  fit_gls <- nlme::gls(FEV1 ~ ARMCD, data_full, correlation = nlme::corAR1(form = ~ VISITN | USUBJID))
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR0")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-4, ignore_attr = TRUE)
  coef_obj <- clubSandwich::coef_test(fit_gls, expected)
  sfit <- summary(fit)
  result <- sfit$coefficients[, "df", drop = TRUE]
  names(result) <- NULL
  expect_equal(result, coef_obj[, "df_Satt"], tolerance = 1e-4)
})

test_that("Empirical works as expected for weighted ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(
    formula = formula, data = data_full, vcov = "Empirical",
    method = "Satterthwaite", weights = data_full$WEIGHT
  )
  # the weights are different in gls and mmrm/SAS;
  data_full$WEIGHT2 <- 1 / data_full$WEIGHT
  fit_gls <- nlme::gls(
    FEV1 ~ ARMCD, data_full,
    correlation = nlme::corAR1(form = ~ VISITN | USUBJID),
    weights = nlme::varFixed(~WEIGHT2)
  )
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR0")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-4, ignore_attr = TRUE)

  coef_obj <- clubSandwich::coef_test(fit_gls, expected)
  sfit <- summary(fit)
  result <- sfit$coefficients[, "df", drop = TRUE]
  names(result) <- NULL
  expect_equal(result, coef_obj[, "df_Satt"], tolerance = 1e-4)
})


## jackknife Satterthwaite vs gls/clubSandwich ----

test_that("Jackknife works as expected for ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(formula = formula, data = data_full, vcov = "Empirical-Jackknife", method = "Satterthwaite")
  fit_gls <- nlme::gls(FEV1 ~ ARMCD, data_full, correlation = nlme::corAR1(form = ~ VISITN | USUBJID))
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR3")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-4, ignore_attr = TRUE)
  coef_obj <- clubSandwich::coef_test(fit_gls, expected)
  sfit <- summary(fit)
  result <- sfit$coefficients[, "df", drop = TRUE]
  names(result) <- NULL
  expect_equal(result, coef_obj[, "df_Satt"], tolerance = 1e-4)
})

test_that("Jackknife works as expected for weighted ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(
    formula = formula, data = data_full, vcov = "Empirical-Jackknife",
    method = "Satterthwaite", weights = data_full$WEIGHT
  )
  # the weights are different in gls and mmrm/SAS;
  data_full$WEIGHT2 <- 1 / data_full$WEIGHT
  fit_gls <- nlme::gls(
    FEV1 ~ ARMCD, data_full,
    correlation = nlme::corAR1(form = ~ VISITN | USUBJID),
    weights = nlme::varFixed(~WEIGHT2)
  )
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR3")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-4, ignore_attr = TRUE)

  coef_obj <- clubSandwich::coef_test(fit_gls, expected)
  sfit <- summary(fit)
  result <- sfit$coefficients[, "df", drop = TRUE]
  names(result) <- NULL
  expect_equal(result, coef_obj[, "df_Satt"], tolerance = 1e-4)
})


## Bias-Reduced Satterthwaite vs gls/clubSandwich ----

test_that("Bias-Reduced works as expected for ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(formula = formula, data = data_full, vcov = "Empirical-Bias-Reduced", method = "Satterthwaite")
  fit_gls <- nlme::gls(FEV1 ~ ARMCD, data_full, correlation = nlme::corAR1(form = ~ VISITN | USUBJID))
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR2")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-3, ignore_attr = TRUE)
  coef_obj <- clubSandwich::coef_test(fit_gls, expected)
  sfit <- summary(fit)
  result <- sfit$coefficients[, "df", drop = TRUE]
  names(result) <- NULL
  expect_equal(result, coef_obj[, "df_Satt"], tolerance = 1e-4)
})

test_that("Bias-Reduced works as expected for weighted ar1", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  fit <- mmrm(
    formula = formula, data = data_full, vcov = "Empirical-Bias-Reduced",
    method = "Satterthwaite", weights = data_full$WEIGHT
  )
  # the weights are different in gls and mmrm/SAS;
  data_full$WEIGHT2 <- 1 / data_full$WEIGHT
  fit_gls <- nlme::gls(
    FEV1 ~ ARMCD, data_full,
    correlation = nlme::corAR1(form = ~ VISITN | USUBJID),
    weights = nlme::varFixed(~WEIGHT2)
  )
  expected <- clubSandwich::vcovCR(fit_gls, type = "CR2")
  expect_equal(fit$beta_vcov_adj, expected, tolerance = 1e-3, ignore_attr = TRUE)

  coef_obj <- clubSandwich::coef_test(fit_gls, expected)
  sfit <- summary(fit)
  result <- sfit$coefficients[, "df", drop = TRUE]
  names(result) <- NULL
  expect_equal(result, coef_obj[, "df_Satt"], tolerance = 1e-4)
})
