test_that(paste(
  "nlme_wrapper_fun produces treatment effect estimates close to ground truth",
  "in large samples with unstructured covariance matrix"
), {
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  nlme_fit <- nlme_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  trt_effect_est <- as.numeric(coef(nlme_fit$fit)["trt"])
  expect_equal(trt_effect_est, 0, tolerance = 0.05)
})

test_that(paste(
  "nlme_wrapper_fun produces treatment effect estimates close to ground truth",
  "in large samples with heterogeneous compound symmetric covariance matrix"
), {
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  nlme_fit <- nlme_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "csh"
  )

  trt_effect_est <- as.numeric(coef(nlme_fit$fit)["trt"])
  expect_equal(trt_effect_est, 0, tolerance = 0.05)
})

test_that(paste(
  "nlme_wrapper_fun produces a fit time estimate"
), {
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
  )
  nlme_fit <- nlme_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  fit_time <- nlme_fit$fit_time
  expect_equal(is.numeric(fit_time), TRUE)
})


test_that(paste(
  "nlme_wrapper_fun handles convergence failures gracefully"
), {
  set.seed(61234)
  dgp_output <- rct_dgp_fun(
    n_obs = 5,
    outcome_covar_mat = diag(10),
  )
  expect_silent(
    nlme_wrapper_fun(
      participant = dgp_output$participant,
      visit_num = dgp_output$visit_num,
      base_bcva = dgp_output$base_bcva,
      strata = dgp_output$strata,
      trt = dgp_output$trt,
      bcva_change = dgp_output$bcva_change,
      covar_type = "us"
    )
  )
})
