# component ----

## overall ----

test_that("component works as expected", {
  object_mmrm_tmb <- get_mmrm_tmb()

  expect_equal(
    names(component(object_mmrm_tmb)),
    c(
      "cov_type", "n_theta", "n_subjects", "n_timepoints",
      "n_obs", "beta_vcov", "varcor", "formula", "dataset",
      "reml", "convergence", "evaluations",
      "conv_message", "call", "theta_est",
      "beta_est", "beta_est_complete", "x_matrix", "y_vector", "neg_log_lik",
      "jac_list", "theta_vcov"
    )
  )

  expect_numeric(component(object_mmrm_tmb, "n_theta"), lower = 0)

  test_list <- component(object_mmrm_tmb,
    name = c("n_theta", "NULL", "x_matrix", "varcor")
  )

  expect_list(test_list, len = 3)

  expect_named(test_list, c("n_theta", "x_matrix", "varcor"))

  expect_identical(component(object_mmrm_tmb, "varcor"), component(object_mmrm_tmb)$varcor)
})

## best_est ----

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
  object_mmrm_tmb <- h_mmrm_tmb(formula, data)

  result <- component(object_mmrm_tmb, "beta_est_complete")
  expect_named(result, c("(Intercept)", "RACEBlack or African American", "RACEWhite", "ones"))
  expect_true(is.na(result["ones"]))
  expect_false(any(is.na(result[-4])))
  expect_named(
    component(object_mmrm_tmb, "beta_est"),
    c("(Intercept)", "RACEBlack or African American", "RACEWhite")
  )
})
