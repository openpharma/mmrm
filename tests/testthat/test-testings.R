# df_1d

test_that("df_1d/df_md use correct function on different methods and give correct result", {
  object <- get_mmrm()
  contrast <- rep(0, length(object$beta_est))
  contrast[3] <- 1
  expect_identical(df_1d(object, contrast), h_df_1d_sat(object, contrast))
  contrast_md <- matrix(0, nrow = 2, ncol = length(object$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 3] <- 1
  expect_identical(df_md(object, contrast_md), h_df_md_sat(object, contrast_md))

  object_kr <- get_mmrm_kr()
  contrast <- rep(0, length(object_kr$beta_est))
  contrast[2] <- 1
  expect_identical(df_1d(object_kr, contrast), h_df_1d_kr(object_kr, contrast))

  contrast_md <- matrix(0, nrow = 2, ncol = length(object_kr$beta_est))
  contrast_md[2, 1] <- 1
  contrast_md[1, 2] <- 1
  expect_identical(df_md(object_kr, contrast_md), h_df_md_kr(object_kr, contrast_md))
})