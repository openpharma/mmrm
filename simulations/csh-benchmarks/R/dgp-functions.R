################################################################################
## Data-Generating Processes
################################################################################

balanced_dgp_fun <- function(
  num_part = 100,
  n_rep_meas = 5,
  fixed_int = 1,
  fixed_trt = 1,
  fixed_time = 1,
  fixed_time_treat = 1,
  fixed_base_cov_1 = 1,
  fixed_base_cov_2 = 1,
  part_effect_var = 1,
  outcome_vars = rep(1, n_rep_meas),
  outcome_cor = 0.5
) {

  ## form a balanced data.frame
  cov_df <- data.frame(
    participant = seq_len(num_part),
    base_cov_1 = rnorm(num_part),
    base_cov_2 = rnorm(num_part),
    trt = c(rep(0, round(num_part / 2)), rep(1, num_part - round(num_part / 2)))
  )
  time_point_df <- expand.grid(
    time = seq_len(n_rep_meas),
    participant = seq_len(num_part)
  )
  df <- cov_df %>% left_join(time_point_df, by = "participant")

  ## produce the model matrix for the fixed effects
  fixed_model_mat <- model.matrix(
    ~ base_cov_1 + base_cov_2 + trt + time + trt:time, data = df
  )

  ## define the full model formula
  full_formula <-
    "y ~ base_cov_1 + base_cov_2 + trt + time + trt:time + (1 | participant)"

  ## produce the random effects model matrix
  df$y <- 1
  rand_model_mat <- lme4::lFormula(eval(full_formula), df)
  rand_model_mat <- t(as.matrix(rand_model_mat$reTrms$Zt))

  ## simulate the random intercept for each participant
  u <- rnorm(num_part, 0, part_effect_var)

  ## define the repeated measures correlation structure, assuming
  ## heterogeneous compound symmetry
  csh_mat <- tcrossprod(sqrt(outcome_vars), sqrt(outcome_vars)) * outcome_cor
  diag(csh_mat) <- outcome_vars

  ## generate the outcomes
  beta <- c(
    fixed_int, fixed_base_cov_1, fixed_base_cov_2, fixed_trt, fixed_time,
    fixed_time_treat
  )
  df$y <- fixed_model_mat %*% beta + rand_model_mat %*% u +
    as.vector(t(MASS::mvrnorm(num_part, rep(0, n_rep_meas), csh_mat)))

  ## return the generated data
  return(list(
    participant = df$participant,
    time = df$time,
    y = df$y,
    trt = df$trt,
    base_cov_1 = df$base_cov_1,
    base_cov_2 = df$base_cov_2

  ))
}
