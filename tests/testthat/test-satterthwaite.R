# h_covbeta_fun ----

test_that("h_covbeta_fun works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  model <- h_mmrm_tmb(formula, fev_data)
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

# h_quad_form_vec ----

test_that("h_quad_form_vec works as expected", {
  x <- 1:2
  mat <- matrix(1:4, 2, 2)
  result <- expect_silent(h_quad_form_vec(x, mat))
  expected <- as.numeric(t(x) %*% mat %*% x)
  expect_equal(result, expected)
})

# h_gradient ----

test_that("h_gradient works as expected", {
  jac_list <- list(
    matrix(1:4, 2, 2),
    matrix(5:8, 2, 2)
  )
  contrast <- c(1:2)
  result <- expect_silent(h_gradient(jac_list, contrast))
  expected <- c(
    t(contrast) %*% jac_list[[1L]] %*% contrast,
    t(contrast) %*% jac_list[[2L]] %*% contrast
  )
  expect_equal(result, expected)
})

# h_df_1d_list ----

test_that("h_df_1d_list works as expected", {
  result <- expect_silent(h_df_1d_list(est = 5, var = 4, v_num = 1, v_denom = 2))
  expected <- list(
    est = 5,
    se = 2,
    df = 1 / 2,
    t_stat = 5 / 2,
    p_val = 2 * pt(q = 5 / 2, df = 1 / 2, lower.tail = FALSE)
  )
  expect_equal(result, expected)
})

# df_1d ----

test_that("df_1d works as expected", {
  object <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  # See design/SAS/sas_log_simple_reml.txt for the source of numbers.
  result <- expect_silent(df_1d(object, 1))
  expect_list(result)
  expect_equal(result$est, 42.8338, tolerance = 1e-4)
  expect_equal(result$se, 0.3509, tolerance = 1e-4)
  expect_identical(round(result$df), 171)
  expect_equal(result$t_stat, 122.07, tolerance = 1e-4)
  expect_true(result$p_val < 0.0001)
})
