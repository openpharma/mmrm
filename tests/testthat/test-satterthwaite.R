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

# h_jac_col_as_matrix ----

test_that("h_jac_col_as_matrix works as expected", {
  jac_matrix <- matrix(1:81, 9, 9)
  result <- expect_silent(h_jac_col_as_matrix(jac_matrix, 5))
  expected <- matrix(37:45, 3, 3)
  expect_identical(result, expected)
})

# h_jac_list ----

test_that("h_jac_list works as expected", {
  # Take a very simple function such that we know what are the derivatives.
  covbeta_fun <- function(theta) {
    matrix(1:4, 2, 2) + matrix(5:8, 2, 2) * theta
  }
  theta_est <- rep(0, 4) # Does not matter here.
  result <- expect_silent(h_jac_list(covbeta_fun, theta_est))
  expect_list(result, len = 4L)
  expect_equal(result[[1L]], matrix(c(5, 0, 0, 0), 2, 2))
  expect_equal(result[[2L]], matrix(c(0, 6, 0, 0), 2, 2))
  expect_equal(result[[3L]], matrix(c(0, 0, 7, 0), 2, 2))
  expect_equal(result[[4L]], matrix(c(0, 0, 0, 8), 2, 2))
})
