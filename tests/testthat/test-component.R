
# component ----

test_that("component works as expected", {
  object_mmrm_tmb <- get_mmrm_tmb()
  object_mmrm <- get_mmrm()

  expect_equal(names(component(object_mmrm_tmb)),
               c("cov_type", "n_theta", "n_subjects", "n_timepoints",
                 "n_obs", "vcov", "varcor", "formula", "dataset",
                 "reml", "convergence", "evaluations",
                 "conv_message", "call", "theta_est",
                 "beta_est", "x_matrix", "y_vector", "neg_log_lik",
                 "jac_list", "theta_vcov"))
  expect_equal(names(component(object_mmrm)),
               c("cov_type", "n_theta", "n_subjects", "n_timepoints",
                 "n_obs", "vcov", "varcor", "formula", "dataset",
                 "reml", "convergence", "evaluations",
                 "conv_message", "call",  "theta_est",
                 "beta_est", "x_matrix", "y_vector", "neg_log_lik",
                 "jac_list", "theta_vcov"))

  expect_numeric(component(object_mmrm_tmb, "n_theta"), lower = 0)

  expect_list(component(object_mmrm_tmb, c("n_theta", "NULL", "x_matrix")),
              len = 2)
  expect_list(component(object_mmrm_tmb, c("n_theta", "NULL", "x_matrix", "varcor")),
              len = 3)
  expect_named(component(object_mmrm_tmb, c("n_theta", "NULL", "x_matrix", "varcor")),
               c("n_theta", "x_matrix", "varcor"))

  expect_identical(component(object_mmrm, "varcor"), component(object_mmrm)$varcor)
})
