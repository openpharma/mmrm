test_that("coconvergence_rate_rate_fun computes DGP-specific convergence rate", {

  ## generate some data from two different DGPs
  set.seed(510)
  no_eff_us <- rct_dgp_fun(
    n_obs = 1000,
    outcome_covar_mat= compute_unstructured_matrix()
  )
  eff_us <- rct_dgp_fun(
    n_obs = 1000,
    outcome_covar_mat = compute_unstructured_matrix(),
    trt_visit_coef = 0.25
  )

  ## fit all methods to each
  mmrm_no_eff <- mmrm_wrapper_fun(
    participant = no_eff_us$participant,
    trt = no_eff_us$trt,
    visit_num = no_eff_us$visit_num,
    base_bcva = no_eff_us$base_bcva,
    strata = no_eff_us$strata,
    bcva_change = no_eff_us$bcva_change,
    covar_type = "us"
  )
  mmrm_eff <- mmrm_wrapper_fun(
    participant = eff_us$participant,
    trt = eff_us$trt,
    visit_num = eff_us$visit_num,
    base_bcva = eff_us$base_bcva,
    strata = eff_us$strata,
    bcva_change = eff_us$bcva_change,
    covar_type = "us"
  )
  glmmtmb_no_eff <- glmmtmb_wrapper_fun(
    participant = no_eff_us$participant,
    trt = no_eff_us$trt,
    visit_num = no_eff_us$visit_num,
    base_bcva = no_eff_us$base_bcva,
    strata = no_eff_us$strata,
    bcva_change = no_eff_us$bcva_change,
    covar_type = "us"
  )
  glmmtmb_eff <- glmmtmb_wrapper_fun(
    participant = eff_us$participant,
    trt = eff_us$trt,
    visit_num = eff_us$visit_num,
    base_bcva = eff_us$base_bcva,
    strata = eff_us$strata,
    bcva_change = eff_us$bcva_change,
    covar_type = "us"
  )
  nlme_no_eff <- nlme_wrapper_fun(
    participant = no_eff_us$participant,
    trt = no_eff_us$trt,
    visit_num = no_eff_us$visit_num,
    base_bcva = no_eff_us$base_bcva,
    strata = no_eff_us$strata,
    bcva_change = no_eff_us$bcva_change,
    covar_type = "us"
  )
  nlme_eff <- nlme_wrapper_fun(
    participant = eff_us$participant,
    trt = eff_us$trt,
    visit_num = eff_us$visit_num,
    base_bcva = eff_us$base_bcva,
    strata = eff_us$strata,
    bcva_change = eff_us$bcva_change,
    covar_type = "us"
  )
  proc_mixed_no_eff <- proc_mixed_wrapper_fun(
    participant = no_eff_us$participant,
    trt = no_eff_us$trt,
    visit_num = no_eff_us$visit_num,
    base_bcva = no_eff_us$base_bcva,
    strata = no_eff_us$strata,
    bcva_change = no_eff_us$bcva_change,
    covar_type = "us"
  )
  proc_mixed_eff <- proc_mixed_wrapper_fun(
    participant = eff_us$participant,
    trt = eff_us$trt,
    visit_num = eff_us$visit_num,
    base_bcva = eff_us$base_bcva,
    strata = eff_us$strata,
    bcva_change = eff_us$bcva_change,
    covar_type = "us"
  )

  ## construct a pseudo-fit_results tibble
  no_eff_us_df <- assemble_df(
    participant = no_eff_us$participant,
    trt = no_eff_us$trt,
    visit_num = no_eff_us$visit_num,
    base_bcva = no_eff_us$base_bcva,
    strata = no_eff_us$strata,
    bcva_change = no_eff_us$bcva_change,
    participant_as_factor = TRUE,
    visit_num_as_factor = TRUE
  )
  eff_us_df <- assemble_df(
    participant = eff_us$participant,
    trt = eff_us$trt,
    visit_num = eff_us$visit_num,
    base_bcva = eff_us$base_bcva,
    strata = eff_us$strata,
    bcva_change = eff_us$bcva_change,
    participant_as_factor = TRUE,
    visit_num_as_factor = TRUE
  )
  fit_results <- tibble(
    .dgp_name = rep(c("no_eff", "eff"), each = 4),
    .method_name = rep(c("mmrm", "glmmtmb", "nlme", "proc_mixed"), 2),
    n_obs = 100,
    fit = list(mmrm_no_eff$fit, glmmtmb_no_eff$fit, nlme_no_eff$fit,
               proc_mixed_no_eff$fit, mmrm_eff$fit, glmmtmb_eff$fit,
               nlme_eff$fit, proc_mixed_eff$fit),
    conv_status_df = list(NULL, NULL, NULL, proc_mixed_no_eff$conv_status_df,
                          NULL, NULL, NULL, proc_mixed_eff$conv_status_df),
    fit_time = rep(1, 8)
  )

  # calculte the convergence_rate tibble
  convergence_rate_tbl <- convergence_rate_fun(fit_results)

  # make sure that all required columns are present
  expect_equal(
    colnames(convergence_rate_tbl),
    c(".dgp_name", ".method_name", "n_obs", "convergence_rate")
  )

  # ensure that the convergence_rate column is numeric
  expect_equal(is.numeric(convergence_rate_tbl$convergence_rate), TRUE)

})
