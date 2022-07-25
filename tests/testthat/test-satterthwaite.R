# h_covbeta_fun ----

test_that("h_covbeta_fun works as expected", {
  model <- get_mmrm_tmb()
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
  vec <- 1:2
  center <- matrix(1:4, 2, 2)
  result <- expect_silent(h_quad_form_vec(vec, center))
  expected <- as.numeric(t(vec) %*% center %*% vec)
  expect_equal(result, expected)
})

# h_quad_form_mat ----

test_that("h_quad_form_mat works as expected for a true row vector as mat", {
  mat <- t(1:2)
  center <- matrix(1:4, 2, 2)
  result <- expect_silent(h_quad_form_mat(mat, center))
  expected <- mat %*% center %*% t(mat)
  expect_equal(result, expected)
})

test_that("h_quad_form_mat works as expected for a larger mat", {
  mat <- matrix(2:7, 3, 2)
  center <- matrix(1:4, 2, 2)
  result <- expect_silent(h_quad_form_mat(mat, center))
  expected <- mat %*% center %*% t(mat)
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

test_that("df_1d works as expected for singular fits", {
  dat <- fev_data
  dat$ones <- 1
  object <- mmrm(
    formula = FEV1 ~ ones + us(AVISIT | USUBJID),
    data = dat
  )
  object2 <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(df_1d(object, 1))
  expected <- expect_silent(df_1d(object2, 1))
  expect_identical(result, expected)
})

# h_md_denom_df ----

test_that("h_md_denom_df works as expected in the standard case", {
  t_stat_df <- c(3, 5, 10)
  result <- expect_silent(h_md_denom_df(t_stat_df))
  n <- 3
  e_sum <- sum(t_stat_df / (t_stat_df - 2))
  expected <- 2 * e_sum / (e_sum - n)
  expect_equal(result, expected)
})

test_that("h_md_denom_df works as expected for a single t-statistic", {
  t_stat_df <- 22
  result <- expect_silent(h_md_denom_df(t_stat_df))
  expected <- t_stat_df
  expect_identical(result, expected)
})

test_that("h_md_denom_df works as expected when the t-statistics are almost identical", {
  t_stat_df <- c(10 + 1e-10, 10 + 2e-10, 10 + 3e-10)
  result <- expect_silent(h_md_denom_df(t_stat_df))
  expected <- 10 + 2e-10 # This is the mean of `t_stat_df`.
  expect_identical(result, expected)
})

test_that("h_md_denom_df works as expected when one t-statistic is 2 or smaller", {
  expect_identical(h_md_denom_df(c(1.9, 5, 10, 15)), 2)
  expect_identical(h_md_denom_df(c(2, 5, 10, 15)), 2)
  expect_false(identical(h_md_denom_df(c(2.1, 5, 10, 15)), 2))
})

# h_df_md_list ----

test_that("h_df_md_list works as expected", {
  result <- expect_silent(h_df_md_list(f_stat = 0.38, num_df = 1, denom_df = 166))
  expected <- list(
    num_df = 1,
    denom_df = 166,
    f_stat = 0.38,
    p_val = 0.5406
  )
  expect_equal(result, expected, tolerance = 1e-2)
})

# h_df_md_from_1d ----

test_that("h_df_md_from_1d works as expected", {
  object <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(h_df_md_from_1d(object, 1))
  expect_list(result)
  expect_named(result, c("num_df", "denom_df", "f_stat", "p_val"))
})

# df_md ----

test_that("df_md works as expected", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  # See design/SAS/sas_log_simple_reml.txt for the source of numbers.
  result <- expect_silent(df_md(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(round(result$denom_df), 166)
  expect_equal(result$f_stat, 36.92, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("df_md works as expected with a non-full rank contrast matrix", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[2, 5] <- 1 # So the first row is all 0s still.
  result <- expect_silent(df_md(object, contrast))
  expected <- df_md(object, contrast[2L, , drop = FALSE])
  expect_equal(result, expected)
})

test_that("df_md works as expected for rank deficient model", {
  object <- get_mmrm_rank_deficient()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  result <- expect_silent(df_md(object, contrast))
  object2 <- get_mmrm()
  expected <- expect_silent(df_md(object2, contrast))
  expect_identical(result, expected)
})
