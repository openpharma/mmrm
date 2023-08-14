# h_df_1d_bw ----

test_that("h_df_1d_bw works as expected", {
  object <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(h_df_1d_bw(object, 1))
  expect_list(result)
  expect_equal(result$est, 42.8338, tolerance = 1e-4)
  expect_equal(result$se, 0.3509, tolerance = 1e-4)
  expect_identical(round(result$df), 196)
  expect_equal(result$t_stat, 122.07, tolerance = 1e-4)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_1d_bw works as expected for singular fits", {
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
  result <- expect_silent(h_df_1d_bw(object, 1))
  expected <- expect_silent(h_df_1d_bw(object2, 1))
  expect_identical(result, expected)
})


# h_df_md_bw ----

test_that("h_df_md_bw works as expected - between effect", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(round(result$denom_df), 192)
  expect_equal(result$f_stat, 36.91, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_bw works as expected - within effect", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 6] <- contrast[2, 7] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(round(result$denom_df), 334)
  expect_equal(result$f_stat, 80.96, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_bw works as expected - both effects", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- contrast[1, 6] <- contrast[2, 7] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(round(result$denom_df), 192)
  expect_equal(result$f_stat, 117.65, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_bw works as expected for rank deficient model", {
  object <- get_mmrm_rank_deficient()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  object2 <- get_mmrm()
  expected <- expect_silent(h_df_md_bw(object2, contrast))
  expect_identical(result, expected)
})
