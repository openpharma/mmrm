test_that("get_mmrm_trt_visit_num_ests extracts interaction estimates", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  mmrm_fit <- mmrm_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ests <- get_mmrm_trt_visit_num_ests(mmrm_fit$fit)
  expect_equal(ests, c(0.5, 1.0, 1.5), tolerance = 0.1)
})

test_that("get_glmm_trt_visit_num_ests extracts interaction estimates", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ests <- get_glmm_trt_visit_num_ests(glmmtmb_fit$fit)
  expect_equal(ests, c(0.5, 1.0, 1.5), tolerance = 0.1)
})

test_that("get_nlme_trt_visit_num_ests extracts interaction estimates", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  nlme_fit <- nlme_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ests <- get_nlme_trt_visit_num_ests(nlme_fit$fit, nlme_fit$data)
  expect_equal(ests, c(0.5, 1.0, 1.5), tolerance = 0.1)
})

test_that("get_mixed_trt_visit_num_ests extracts interaction estimates", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 10000,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ests <- get_mixed_trt_visit_num_ests(proc_mixed_fit$fit)
  expect_equal(ests, c(0.5, 1.0, 1.5), tolerance = 0.1)
})

test_that("get_mmrm_convergence extracts convergence status", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  mmrm_fit <- mmrm_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract convergence status
  conv_status <- get_mmrm_convergence(mmrm_fit$converged)
  expect_equal(conv_status, TRUE)
})

test_that("get_glmm_convergence extracts convergence status", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the glmmtmb with glmmtmb
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract convergence status
  conv_status <- get_glmm_convergence(glmmtmb_fit$fit)
  expect_equal(conv_status, TRUE)
})

test_that("get_mixed_convergence extracts convergence status", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the proc_mixed with proc_mixed
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract convergence status
  conv_status <- get_mixed_convergence(proc_mixed_fit$converged)
  expect_equal(conv_status, TRUE)
})

test_that("get_mmrm_trt_visit_num_ses extracts standard errors", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  mmrm_fit <- mmrm_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ses <- get_mmrm_trt_visit_num_ses(mmrm_fit$fit)
  expect_equal(is.numeric(ses), TRUE)
  expect_equal(length(ses), 3)
})

test_that("get_glmm_trt_visit_num_ses extracts standard errors", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ses <- get_glmm_trt_visit_num_ses(glmmtmb_fit$fit)
  expect_equal(is.numeric(ses), TRUE)
  expect_equal(length(ses), 3)
})

test_that("get_nlme_trt_visit_num_ses extracts standard errors", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  nlme_fit <- nlme_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ses <- get_nlme_trt_visit_num_ses(nlme_fit$fit, nlme_fit$data)
  expect_equal(is.numeric(ses), TRUE)
  expect_equal(length(ses), 3)
})

test_that("get_mixed_trt_visit_num_ses extracts standard errors", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  ses <- get_mixed_trt_visit_num_ses(proc_mixed_fit$fit)
  expect_equal(is.numeric(ses), TRUE)
  expect_equal(length(ses), 3)
})

test_that("get_mmrm_trt_visit_num_pvals extracts nominal p-values", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  mmrm_fit <- mmrm_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  pvals <- get_mmrm_trt_visit_num_pvals(mmrm_fit$fit)
  expect_equal(is.numeric(pvals), TRUE)
  expect_equal(length(pvals), 3)
})

test_that("get_glmm_trt_visit_num_pvals extracts nominal p-values", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  glmmtmb_fit <- glmmtmb_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  pvals <- get_glmm_trt_visit_num_pvals(glmmtmb_fit$fit)
  expect_equal(is.numeric(pvals), TRUE)
  expect_equal(length(pvals), 3)
})

test_that("get_nlme_trt_visit_num_pvals extracts nominal p-values", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  nlme_fit <- nlme_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  pvals <- get_nlme_trt_visit_num_pvals(nlme_fit$fit, nlme_fit$data)
  expect_equal(is.numeric(pvals), TRUE)
  expect_equal(length(pvals), 3)
})

test_that("get_mixed_trt_visit_num_pvals extracts nominal p-values", {
  # generate data
  set.seed(51235)
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
    trt_visit_coef = 0.5
  )

  # fit the mmrm with mmrm
  proc_mixed_fit <- proc_mixed_wrapper_fun(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    covar_type = "us"
  )

  # extract estimates
  pvals <- get_mixed_trt_visit_num_pvals(proc_mixed_fit$fit)
  expect_equal(is.numeric(pvals), TRUE)
  expect_equal(length(pvals), 3)
})