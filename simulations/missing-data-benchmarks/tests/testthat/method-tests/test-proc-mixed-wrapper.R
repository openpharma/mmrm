test_that(paste(
  "proc_mixed_wrapper_fun produces treatment effect estimates close to ground",
  "truth in large samples with unstructure covariance matrix"
), {
  skip_if_not(run_sas_tests)

  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  expect_true("data" %in% names(proc_mixed_fit))
  trt_effect_ests <- proc_mixed_fit$fit$Estimate
  expect_equal(trt_effect_ests, c(0, 0, 0), tolerance = 0.05)
})

test_that(paste(
  "proc_mixed_wrapper_fun produces treatment effect estimates close to ground",
  "truth in large samples with heterogeneous compound symmetry covariance",
  "matrix"
), {
  skip_if_not(run_sas_tests)
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "csh"
  )

  trt_effect_ests <- proc_mixed_fit$fit$Estimate
  expect_equal(trt_effect_ests, c(0, 0, 0), tolerance = 0.05)
})

test_that(paste(
  "proc_mixed_wrapper_fun produces treatment effect estimates close to ground",
  "truth in large samples with heterogeneous Toeplitz covariance matrix"
), {
  skip_if_not(run_sas_tests)
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "toeph"
  )

  trt_effect_ests <- proc_mixed_fit$fit$Estimate
  expect_equal(trt_effect_ests, c(0, 0, 0), tolerance = 0.05)
})

test_that(paste(
  "proc_mixed_wrapper_fun produces a fit time"
), {
  skip_if_not(run_sas_tests)
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  fit_time <- proc_mixed_fit$fit_time
  expect_equal(fit_time, 1.35, tolerance = 0.1)
})
