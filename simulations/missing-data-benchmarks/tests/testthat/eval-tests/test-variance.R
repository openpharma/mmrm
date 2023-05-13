test_that("variance_fun computes DGP-specific variance", {
  ## generate some data from two different DGPs
  set.seed(510)
  no_eff_us <- rct_dgp_fun(
    n_obs = 1000,
    outcome_covar_mat = compute_unstructured_matrix()
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
    fit = list(
      mmrm_no_eff$fit, glmmtmb_no_eff$fit, nlme_no_eff$fit,
      proc_mixed_no_eff$fit, mmrm_eff$fit, glmmtmb_eff$fit,
      nlme_eff$fit, proc_mixed_eff$fit
    ),
    data = list(
      NULL, NULL, no_eff_us_df, NULL,
      NULL, NULL, eff_us_df, NULL
    ),
    fit_time = rep(1, 8)
  )

  # calculte the variance tibble
  variance_tbl <- variance_fun(fit_results)

  # make sure that all required columns are present
  expect_equal(
    colnames(variance_tbl),
    c(".dgp_name", ".method_name", "n_obs", "coefficient", "variance")
  )

  # make sure that all of the coefficients represented
  expect_equal(
    unique(variance_tbl$coefficient),
    paste0("trt_visit_num", str_pad(seq_len(10), width = 2, pad = "0"))
  )

  # ensure that the variance column is numeric
  expect_equal(is.numeric(variance_tbl$variance), TRUE)
})
