# Simulate data from an ophthalmology randomized control trial. The number of
# patients enrolled is specified by the n_obs argument, and the true repeated
# measures covariance matrix must be specified. The effect of the treatment,
# visit number, and treatment by visit interactions can also be specified,
# Finally, the amount of dropout is dictated by the missingness argument. If
# left at its default value, NULL, no dropout occurs. Other options include,
# "low", "high", and "extreme". The ouput is a list, in long format, of the
# simulated trial data. This is because simChef requires DGP functions to return
# list objects.
rct_dgp_fun <- function(
    n_obs,
    outcome_covar_mat,
    trt_coef = 0,
    visit_coef = 0.25,
    trt_visit_coef = 0,
    missingness = NULL) {
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

  # assemble into a data.frame
  df <- data.frame(
    participant = covars_df$participant,
    bcva_change = bcva_change,
    base_bcva = covars_df$base_bcva,
    strata = covars_df$strata,
    trt = covars_df$trt,
    visit_num = covars_df$visit_num
  )

  # delete observations at random if asked
  if (!is.null(missingness)) {
    df <- missing_at_random(df, type = missingness)
  }

  # return the list
  return(as.list(df))
}
