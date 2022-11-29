################################################################################
## Data-Generating Processes
################################################################################


#' Assemble the true heterogeneous compound symmetry matrix
#'
#' @description Computes the true heterogeneous compound symmetry covariance
#'   matrix based on the specified outcome variances and the correlation
#'   parameter.
#'
#' @param vars A numeric vector corresponding to the repeated measures
#'   variances.
#' @param corr A numeric corresponding to the correlation parameter of the
#'   heterogeneous compound symmetry covariance structure.
#'
#' @return The true heterogeneous compound symmetry covariance matrix used to
#'   generate the data.
compute_true_covar_mat <- function(vars, corr) {
  csh_mat <- tcrossprod(sqrt(vars), sqrt(vars)) * corr
  diag(csh_mat) <- vars
  return(csh_mat)
}

#' A Simple RCT Data-Generating Function
#'
#' @description This function simulates a randomized control trial with
#'   participants equally split across the control and treatment arms.
#'   Participants' repeated measures are assumed to be normally distributed with
#'   heterogeneous compound symmetry covariance matrix. Participants' outcomes
#'   are collected at each time point.
#'
#' @param num_part A numeric indicating the total number of participants.
#' @param num_rep_meas A numeric indicating the number of repeated measures.
#' @param fixed_int A numeric corresponding to the fixed intercept effect.
#' @param fixed_trt A numeric corresponding to the fixed treatment effect.
#' @param fixed_time A numeric corresponding to the fixed time point effect.
#' @param fixed_time_trt A numeric corresponding to the fixed time x treatment
#'   interaction effect.
#' @param fixed_base_cov A numeric corresponding to the a baseline covariate's
#'   fixed effect.
#' @param outcome_vars A numeric vector with length equal to num_rep_meas. It is
#'   the vector of the repeated measures' variances.
#' @param outcome_cor A numeric corresponding to the correlation parameter in
#'   the heterogeneous compound symmetry covariance matrix.
#'
#' @return A list containing the simulated participants' IDs, time point
#'   indicators, outcomes, treatment assignments and baseline covariates.

rct_dgp_fun <- function(
  num_part = 100,
  num_rep_meas = 10,
  fixed_int = 1,
  fixed_trt = 1,
  fixed_time = 1,
  fixed_time_trt = 1,
  fixed_base_cov = 1,
  outcome_vars = NULL,
  outcome_cor = 0.5
) {

  compute_true_covar_mat <- function(vars, corr) {
    csh_mat <- tcrossprod(sqrt(vars), sqrt(vars)) * corr
    diag(csh_mat) <- vars
    return(csh_mat)
  }

  if (is.null(outcome_vars)) {
    outcome_vars <- rep(1, num_rep_meas)
  }

  ## form a balanced data.frame
  cov_df <- data.frame(
    participant = seq_len(num_part),
    base_cov = rbinom(num_part, 1, 0.7),
    trt = c(rep(0, round(num_part / 2)), rep(1, num_part - round(num_part / 2)))
  )
  time_point_df <- expand.grid(
    time = seq_len(num_rep_meas),
    participant = seq_len(num_part)
  )
  df <- cov_df %>% dplyr::left_join(time_point_df, by = "participant")

  ## produce the model matrix for the fixed effects
  fixed_model_mat <- model.matrix(
    ~ base_cov + trt*time, data = df
  )

  ## define the full model formula
  full_formula <-
    "y ~ base_cov + trt*time + (time | participant)"

  ## define the repeated measures correlation structure, assuming
  ## heterogeneous compound symmetry
  csh_mat <- compute_true_covar_mat(outcome_vars, outcome_cor)

  ## generate the outcomes
  beta <- c(
    fixed_int, fixed_base_cov, fixed_trt, fixed_time, fixed_time_trt
  )
  df$y <- fixed_model_mat %*% beta +
    as.vector(t(MASS::mvrnorm(num_part, rep(0, num_rep_meas), csh_mat)))

  ## return the generated data
  return(list(
    participant = as.factor(df$participant),
    time = as.factor(df$time),
    y = df$y,
    trt = df$trt,
    base_cov = df$base_cov
  ))
}
