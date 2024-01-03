# h_jac_list ----

test_that("h_jac_list works as expected", {
  # Take a spatial exponential because the number of parameters is small.
  fit <- get_mmrm_spatial()
  theta_est <- c(4, 0)
  beta_vcov <- matrix(c(1, 0, 0, 1), ncol = 2)
  result <- expect_silent(h_jac_list(fit$tmb_data, theta_est, beta_vcov))
  expect_list(result, len = 2L)
  expect_equal(result[[1L]], matrix(c(6.037242, 2.893871, 2.893871, 2.893871), 2, 2), tolerance = 1e-4)
  expect_equal(result[[2L]], matrix(c(1.495479, 0.744836, 0.744836, 0.744836), 2, 2), tolerance = 1e-4)
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

# h_df_1d_sat ----

test_that("h_df_1d_sat works as expected", {
  object <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  # See design/SAS/sas_log_simple_reml.txt for the source of numbers.
  result <- expect_silent(h_df_1d_sat(object, 1))
  expect_list(result)
  expect_equal(result$est, 42.8338, tolerance = 1e-4)
  expect_equal(result$se, 0.3509, tolerance = 1e-4)
  expect_identical(round(result$df), 171)
  expect_equal(result$t_stat, 122.07, tolerance = 1e-4)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_1d_sat works as expected for singular fits", {
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
  result <- expect_silent(h_df_1d_sat(object, 1))
  expected <- expect_silent(h_df_1d_sat(object2, 1))
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

# h_df_md_sat ----

test_that("h_df_md_sat works as expected", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  # See design/SAS/sas_log_simple_reml.txt for the source of numbers.
  result <- expect_silent(h_df_md_sat(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(round(result$denom_df), 166)
  expect_equal(result$f_stat, 36.92, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_sat works as expected with a non-full rank contrast matrix", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[2, 5] <- 1 # So the first row is all 0s still.
  result <- expect_silent(h_df_md_sat(object, contrast))
  expected <- h_df_md_sat(object, contrast[2L, , drop = FALSE])
  expect_equal(result, expected)
})

test_that("h_df_md_sat works as expected for rank deficient model", {
  object <- get_mmrm_rank_deficient()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  result <- expect_silent(h_df_md_sat(object, contrast))
  object2 <- get_mmrm()
  expected <- expect_silent(h_df_md_sat(object2, contrast))
  expect_identical(result, expected)
})
