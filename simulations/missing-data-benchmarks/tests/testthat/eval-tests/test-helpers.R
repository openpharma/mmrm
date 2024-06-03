# get_trt_visit_num_ests ----

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
  skip_if_not(run_sas_tests)
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

# get_convergence ----

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
  skip_if_not(run_sas_tests)
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

# get_trt_visit_num_ses ----

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
  skip_if_not(run_sas_tests)
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

# get_trt_visit_num_pvals ----

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
  skip_if_not(run_sas_tests)
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

# get_emmeans_output ----

test_that("get_mmrm_emmeans_output extracts emmeans contrasts", {
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

  # extract emmeans contrasts estimates
  result <- get_mmrm_emmeans_output(mmrm_fit$fit)
  expect_data_frame(result)
  expect_identical(nrow(result), 3L)
  expected_cols <- c("visit_num", "estimate", "stderr", "df", "tvalue", "pvalue", "lower", "upper")
  expect_named(result, expected_cols)
})

test_that("get_glmm_emmeans_output extracts emmeans contrasts", {
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
  result <- get_glmm_emmeans_output(glmmtmb_fit$fit)
  expect_data_frame(result)
  expect_identical(nrow(result), 3L)
  expected_cols <- c("visit_num", "estimate", "stderr", "df", "tvalue", "pvalue", "lower", "upper")
  expect_named(result, expected_cols)
})

test_that("get_nlme_emmeans_output extracts emmeans contrasts", {
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
  result <- get_nlme_emmeans_output(nlme_fit$fit, nlme_fit$data)
  expect_data_frame(result)
  expect_identical(nrow(result), 3L)
  expected_cols <- c("visit_num", "estimate", "stderr", "df", "tvalue", "pvalue", "lower", "upper")
  expect_named(result, expected_cols)
})

test_that("get_mixed_emmeans_like_output extracts emmeans contrasts", {
  skip_if_not(run_sas_tests)
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
  result <- get_mixed_emmeans_like_output(proc_mixed_fit$fit)
  expect_data_frame(result)
  expect_identical(nrow(result), 3L)
  expected_cols <- c("visit_num", "estimate", "stderr", "df", "tvalue", "pvalue", "lower", "upper")
  expect_named(result, expected_cols)
})

# get_cov_mat_estimate ----

test_that("get_mmrm_cov_mat_estimate works as expected", {
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

  result <- expect_silent(get_mmrm_cov_mat_estimate(mmrm_fit$fit))
  expect_matrix(result, nrows = 3, ncols = 3)
  expect_silent(chol(result))
})

test_that("get_glmm_cov_mat_estimate works as expected", {
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

  result <- expect_silent(get_glmm_cov_mat_estimate(glmmtmb_fit$fit))
  expect_matrix(result, nrows = 3, ncols = 3)
  expect_silent(chol(result))
})

test_that("get_nlme_cov_mat_estimate works as expected", {
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

  result <- expect_silent(get_nlme_cov_mat_estimate(nlme_fit$fit))
  expect_matrix(result, nrows = 3, ncols = 3)
  expect_silent(chol(result))
})

test_that("get_mixed_cov_mat_estimate works as expected", {
  skip_if_not(run_sas_tests)
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
  result <- expect_silent(get_mixed_cov_mat_estimate(proc_mixed_fit$fit))
  expect_matrix(result, nrows = 3, ncols = 3)
  expect_silent(chol(result))
})
