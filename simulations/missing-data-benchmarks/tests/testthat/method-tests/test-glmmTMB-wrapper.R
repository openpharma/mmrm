test_that(paste(
  "glmmTMB_wrapper_fun produces treatment effect estimates close to ground",
  "truth in large samples with unstructure covariance matrix"
), {

  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  trt_effect_est <- as.numeric(fixef(glmmtmb_fit$fit)$cond["trt"])
  expect_equal(trt_effect_est, 0, tolerance = 0.05)
})

test_that(paste(
  "glmmTMB_wrapper_fun produces treatment effect estimates close to ground",
  "truth in large samples with heterogeneous compound symmetry covariance",
  "matrix"
), {

  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "csh"
  )

  trt_effect_est <- as.numeric(fixef(glmmtmb_fit$fit)$cond["trt"])
  expect_equal(trt_effect_est, 0, tolerance = 0.05)
})

test_that(paste(
  "glmmTMB_wrapper_fun produces treatment effect estimates close to ground",
  "truth in large samples with heterogeneous Toeplitz covariance matrix"
), {

  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "toeph"
  )

  trt_effect_est <- as.numeric(fixef(glmmtmb_fit$fit)$cond["trt"])
  expect_equal(trt_effect_est, 0, tolerance = 0.05)
})
