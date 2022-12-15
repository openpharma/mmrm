# df_1d ----

test_that("df_1d use correct function on Satterthewaite method fit", {
  object <- get_mmrm()
  contrast <- rep(0, length(object$beta_est))
  contrast[3] <- 1
  expect_identical(df_1d(object, contrast), h_df_1d_sat(object, contrast))
})

test_that("df_1d use correct function on Kenward-Roger method fit", {
  object_kr <- get_mmrm_kr()
  contrast <- rep(0, length(object_kr$beta_est))
  contrast[2] <- 1
  expect_identical(df_1d(object_kr, contrast), h_df_1d_kr(object_kr, contrast))
})

# df_md ----

test_that("df_md use correct function on Satterthewaite method fit", {
  object <- get_mmrm()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 3] <- 1
  expect_identical(df_md(object, contrast_md), h_df_md_sat(object, contrast_md))
})

test_that("df_md use correct function on Kenward-Roger method fit", {
  object_kr <- get_mmrm_kr()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object_kr$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 2] <- 1
  expect_identical(df_md(object_kr, contrast_md), h_df_md_kr(object_kr, contrast_md))
})

test_that("df_md use correct function on Kenward-Roger-Linear method fit", {
  object_kr <- get_mmrm_krl()
  contrast_md <- matrix(0, nrow = 2, ncol = length(object_kr$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 2] <- 1
  expect_identical(df_md(object_kr, contrast_md), h_df_md_kr(object_kr, contrast_md, linear = TRUE))
})
