rct_dgp_fun <- function(
  n_obs,
  outcome_covar_mat,
  trt_coef = 0,
  visit_coef = 0.25,
  trt_visit_coef = 0
) {
  # generate the covariates
  covars_df <- generate_covariates(
    n_obs = n_obs, n_visits = nrow(outcome_covar_mat)
  )

  # generate the outcomes
  bcva_out <- generate_outcomes(
    covars_df = covars_df,
    cov_mat = outcome_covar_mat,
    trt_coef = trt_coef,
    visit_coef = visit_coef,
    trt_visit_coef = trt_visit_coef
  )

  # compute the change in BCVA
  bcva_change <- bcva_out - covars_df$base_bcva

  # return the list
  return(
    list(
      participant = covars_df$participant,
      bcva_change = bcva_change,
      base_bcva = covars_df$base_bcva,
      strata = covars_df$strata,
      trt = covars_df$trt,
      visit_num = covars_df$visit_num
    )
  )
}