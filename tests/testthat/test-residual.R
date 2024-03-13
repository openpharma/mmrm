# h_df_1d_res ----

test_that("h_df_1d_res works as expected", {
  fit <- get_mmrm()
  contrast <- numeric(length = length(coef(fit)))
  contrast[2] <- 1
  result <- expect_silent(h_df_1d_res(fit, contrast))
  expect_list(result)
  expect_equal(result$est, 1.5305, tolerance = 1e-3)
  expect_equal(result$se, 0.6245, tolerance = 1e-3)
  expect_identical(result$df, 526L)
  expect_equal(result$t_stat, 2.4509, tolerance = 1e-3)
  expect_equal(result$p_val, 0.01458, tolerance = 1e-3)
})

# h_df_md_res ----

test_that("h_df_md_res works as expected", {
  fit <- get_mmrm()
  contrast <- matrix(data = 0, ncol = length(coef(fit)), nrow = 2)
  contrast[1, 2] <- 1
  contrast[2, 3] <- 1
  result <- expect_silent(h_df_md_res(fit, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(result$denom_df, 526L)
  expect_equal(result$f_stat, 36.9114, tolerance = 1e-3)
  expect_true(result$p_val < 1e-10)
})
