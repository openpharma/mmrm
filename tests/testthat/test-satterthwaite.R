# h_covbeta_fun ----

test_that("h_covbeta_fun works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  model <- mmrm_tmb(formula, fev_data)
  result <- expect_silent(h_covbeta_fun(model))
  expect_function(result, args = "theta")
  value <- result(model$theta_est)
  expected_value <- model$beta_vcov
  expect_equal(value, expected_value, ignore_attr = TRUE)
  other_value <- result(model$theta_est * 1.1)
  expect_false(identical(value, other_value))
})
