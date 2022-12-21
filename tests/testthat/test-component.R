# component ----

## overall ----

test_that("component works as expected for mmrm_tmb objects", {
  object_mmrm_tmb <- get_mmrm_tmb()

  expect_equal(
    names(component(object_mmrm_tmb)),
    c(
      "cov_type", "n_theta", "n_subjects", "n_timepoints",
      "n_obs", "beta_vcov", "beta_vcov_complete", "varcor", "formula", "dataset",
      "n_groups", "reml", "convergence", "evaluations", "method",
      "conv_message", "call", "theta_est",
      "beta_est", "beta_est_complete", "beta_aliased",
      "x_matrix", "y_vector", "neg_log_lik",
      "jac_list", "theta_vcov"
    )
  )

  expect_numeric(component(object_mmrm_tmb, "n_theta"), lower = 0)
  expect_identical(component(object_mmrm_tmb, "n_groups"), 1L)
  test_list <- component(object_mmrm_tmb,
    name = c("n_theta", "NULL", "x_matrix", "varcor")
  )

  expect_list(test_list, len = 3)
  expect_named(test_list, c("n_theta", "x_matrix", "varcor"))
  expect_identical(component(object_mmrm_tmb, "varcor"), component(object_mmrm_tmb)$varcor)
  expect_identical(component(object_mmrm_tmb, "beta_vcov"), object_mmrm_tmb$beta_vcov)
  expect_null(component(object_mmrm_tmb, "method"))
})

## beta_vcov ----

test_that("component returns adjusted beta cov matrix for Kenward-Roger adjusted mmrm", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_identical(component(object_mmrm_kr, "beta_vcov"), object_mmrm_kr$beta_vcov_adj)
})

## best_est_complete ----

test_that("component produces complete coefficient vector as expected in full rank model", {
  object_mmrm_tmb <- get_mmrm_tmb()

  result <- component(object_mmrm_tmb, "beta_est_complete")
  expected <- component(object_mmrm_tmb, "beta_est")
  expect_identical(result, expected)
})

test_that("component returns coefficient vectors as expected in rank deficient model", {
  formula <- FEV1 ~ RACE + ones + us(AVISIT | USUBJID)
  data <- fev_data
  data$ones <- 1
  object_mmrm_tmb <- fit_mmrm(formula, data, weights = rep(1, nrow(data)))

  result <- component(object_mmrm_tmb, "beta_est_complete")
  expect_named(result, c("(Intercept)", "RACEBlack or African American", "RACEWhite", "ones"))
  expect_true(is.na(result["ones"]))
  expect_false(any(is.na(result[-4])))
  expect_named(
    component(object_mmrm_tmb, "beta_est"),
    c("(Intercept)", "RACEBlack or African American", "RACEWhite")
  )
})

## best_vcov_complete ----

test_that("component produces complete variance-covariance matrix as expected in full rank model", {
  object_mmrm_tmb <- get_mmrm_tmb()

  result <- component(object_mmrm_tmb, "beta_vcov_complete")
  expected <- component(object_mmrm_tmb, "beta_vcov")
  expect_identical(result, expected)
})

test_that("component returns complete variance-covariance matrix as expected in rank deficient model", {
  formula <- FEV1 ~ RACE + ones + us(AVISIT | USUBJID)
  data <- fev_data
  data$ones <- 1
  object_mmrm_tmb <- fit_mmrm(formula, data, weights = rep(1, nrow(data)))

  result <- component(object_mmrm_tmb, "beta_vcov_complete")
  expect_identical(dim(result), c(4L, 4L))
  expect_names(
    rownames(result),
    identical.to = c("(Intercept)", "RACEBlack or African American", "RACEWhite", "ones")
  )
  expect_true(all(is.na(result["ones", ])))
  expect_true(all(is.na(result[, "ones"])))
  expect_identical(component(object_mmrm_tmb, "beta_vcov"), result[1:3, 1:3])
})
