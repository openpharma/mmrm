
# component ----

test_that("component works as expected", {
  object_mmrm_tmb <- get_mmrm_tmb()

  expect_equal(names(component(object_mmrm_tmb)),
               c("cov_type", "n_theta", "n_subjects", "n_timepoints",
                 "n_obs", "vcov", "varcor", "formula", "dataset",
                 "reml", "convergence", "evaluations",
                 "conv_message", "call", "theta_est",
                 "beta_est", "x_matrix", "y_vector", "neg_log_lik",
                 "jac_list", "theta_vcov"))

  expect_numeric(component(object_mmrm_tmb, "n_theta"), lower = 0)

  test_list <- component(object_mmrm_tmb,
                name = c("n_theta", "NULL", "x_matrix", "varcor"))

  expect_list(test_list, len = 3)

  expect_named(test_list, c("n_theta", "x_matrix", "varcor"))

  expect_identical(component(object_mmrm_tmb, "varcor"), component(object_mmrm_tmb)$varcor)
})
