test_that(paste(
  "rct_dgp_fun with no treatment effect produces a dataset with no significant",
  "treatment effect"
), {
  library(mmrm)

  # generate the data
  set.seed(514)
  covar_mat <- compute_csh_matrix(vars = rep(1, 5), corr = 0.2)
  rct_ls <- rct_dgp_fun(n_obs = 5000, outcome_covar_mat = covar_mat)
  rct_df <- as.data.frame(do.call(cbind, rct_ls))

  # fit the mmrm model
  rct_df$participant <- as.factor(rct_df$participant)
  rct_df$trt <- as.factor(rct_df$trt)
  rct_df$visit_num <- as.factor(rct_df$visit_num)
  rct_df$strata <- as.factor(rct_df$strata)
  mmrm_fit <- mmrm(
    formula = bcva_change ~ base_bcva + strata + trt * visit_num +
      csh(visit_num | participant),
    data = rct_df
  )

  expect_equal(
    mmrm_fit$beta_est["trt1"], 0, tolerance = 0.05, ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num2"], 0, tolerance = 0.05,
    ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num3"], 0, tolerance = 0.05,
    ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num4"], 0, tolerance = 0.05,
    ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num5"], 0, tolerance = 0.05,
    ignore_attr = TRUE
  )
})

test_that(paste(
  "rct_dgp_fun with linear treatment effect produces a dataset with",
  "discernable linear treatment effect"
), {
  library(mmrm)

  # generate the data
  set.seed(62341)
  covar_mat <- compute_csh_matrix(vars = rep(1, 5), corr = 0.1)
  rct_ls <- rct_dgp_fun(
    n_obs = 5000, outcome_covar_mat = covar_mat, trt_coef = 1,
    trt_visit_coef = 0.25
  )
  rct_df <- as.data.frame(do.call(cbind, rct_ls))

  # fit the mmrm model
  rct_df$participant <- as.factor(rct_df$participant)
  rct_df$trt <- as.factor(rct_df$trt)
  rct_df$visit_num <- as.factor(rct_df$visit_num)
  rct_df$strata <- as.factor(rct_df$strata)
  mmrm_fit <- mmrm(
    formula = bcva_change ~ base_bcva + strata + trt * visit_num +
      csh(visit_num | participant),
    data = rct_df
  )

  expect_equal(
    mmrm_fit$beta_est["trt1"] - 1.25, 0, tolerance = 0.1, ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num2"] - 0.25, 0, tolerance = 0.1,
    ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num3"] - 0.50, 0, tolerance = 0.1,
    ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num4"] - 0.75, 0, tolerance = 0.1,
    ignore_attr = TRUE
  )
  expect_equal(
    mmrm_fit$beta_est["trt1:visit_num5"] - 1, 0, tolerance = 0.1,
    ignore_attr = TRUE
  )
})
