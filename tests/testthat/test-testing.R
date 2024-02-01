# df_1d ----

test_that("df_1d uses correct function on Satterthwaite method fit", {
  object <- get_mmrm()
  contrast <- rep(0, length(object$beta_est))
  contrast[3] <- 1
  expect_identical(df_1d(object, contrast), h_df_1d_sat(object, contrast))
})

test_that("df_1d uses correct function on Kenward-Roger method fit", {
  object_kr <- get_mmrm_kr()
  contrast <- rep(0, length(object_kr$beta_est))
  contrast[2] <- 1
  expect_identical(df_1d(object_kr, contrast), h_df_1d_kr(object_kr, contrast))
})

test_that("df_1d uses correct function on Between-Within method fit", {
  object <- get_mmrm_bw()
  contrast <- rep(0, length(object$beta_est))
  contrast[2] <- 1
  expect_identical(df_1d(object, contrast), h_df_1d_bw(object, contrast))
})

# df_md ----

test_that("df_md uses correct function on Satterthwaite method fit", {
  object <- get_mmrm()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 3] <- 1
  expect_identical(df_md(object, contrast_md), h_df_md_sat(object, contrast_md))
})

test_that("df_md uses correct function on Kenward-Roger method fit", {
  object_kr <- get_mmrm_kr()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object_kr$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 2] <- 1
  expect_identical(df_md(object_kr, contrast_md), h_df_md_kr(object_kr, contrast_md))
})

test_that("df_md uses correct function on Kenward-Roger-Linear method fit", {
  object_kr <- get_mmrm_krl()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object_kr$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 2] <- 1
  expect_identical(df_md(object_kr, contrast_md), h_df_md_kr(object_kr, contrast_md))
})

test_that("df_md uses correct function on Between-Within method fit", {
  object_bw <- get_mmrm_bw()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object_bw$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 2] <- 1
  expect_identical(df_md(object_bw, contrast_md), h_df_md_bw(object_bw, contrast_md))
})

test_that("df_md return df=0 and other stat all NA if empty matrix provided", {
  object_bw <- get_mmrm_bw()
  contrast_md <- matrix(0, nrow = 0, ncol = length(object_bw$beta_est))
  expect_identical(
    df_md(object_bw, contrast_md),
    list(num_df = 0, denom_df = NA_real_, f_stat = NA_real_, p_val = NA_real_)
  )
})

# h_test_1d ----

test_that("h_test_1d works as expected", {
  object <- get_mmrm()
  contrast <- rep(0, length(object$beta_est))
  contrast[3] <- 1
  df <- 3.5

  result <- expect_silent(h_test_1d(object, contrast, df))
  expect_list(result)
  expect_named(result, c("est", "se", "df", "t_stat", "p_val"))
  expect_snapshot_tolerance(result)
})

# h_test_md ----

test_that("h_test_md works as expected", {
  object <- get_mmrm()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 3] <- 1
  df <- 5

  result <- expect_silent(h_test_md(object, contrast_md, df))
  expect_list(result)
  expect_named(result, c("num_df", "denom_df", "f_stat", "p_val"))
  expect_snapshot_tolerance(result)
})
