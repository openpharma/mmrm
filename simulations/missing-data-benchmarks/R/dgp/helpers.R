# Generates a data.frame of baseline covariates and treatment indicators for
# n_obs distinct patients. The resulting data.frame is in a "long" format,
# meaning these covariates are repeated n_visits times, where n_visits is the
# number of visits.
generate_covariates <- function(n_obs, n_visits = 10) {
  # participant ID
  participant <- seq_len(n_obs)

  # baseline best corrected visual acuity score
  base_bcva <- rnorm(n = n_obs, mean = 75, sd = 10)

  # statification factor
  strata <- as.vector(
    c(1, 2, 3) %*% rmultinom(n = n_obs, 1, prob = c(0.3, 0.3, 0.4))
  )

  # treatment indicator
  trt <- rbinom(n = n_obs, size = 1, prob = 0.5)

  # visit number
  visit_num <- rep(seq_len(n_visits), n_obs)

  # assemble into a covariates data.frame
  data.frame(
    participant = rep(participant, each = n_visits),
    base_bcva = rep(base_bcva, each = n_visits),
    strata = as.factor(rep(strata, each = n_visits)),
    trt = rep(trt, each = n_visits),
    visit_num
  )
}

# Randomly generate a heterogeneous compound symmetry covariance matix. The
# variances and correlation parameters are set using the vars and corr
# arguments, respectively.
compute_csh_matrix <- function(
    vars = seq(from = 1, by = 0.5, length.out = 10), corr = 0.25) {
  csh_mat <- tcrossprod(sqrt(vars), sqrt(vars)) * corr
  diag(csh_mat) <- vars
  return(csh_mat)
}

# Randomly generate a unstructured covariance matix. The variances are set using
# the vars argument.
compute_unstructured_matrix <- function(
    vars = seq(from = 1, by = 0.5, length.out = 10)) {
  n_visits <- length(vars)
  corr_mat <- abs(cov2cor(
    clusterGeneration::genPositiveDefMat(dim = n_visits)$Sigma
  ))
  sd_mat <- diag(sqrt(vars))
  us_mat <- sd_mat %*% corr_mat %*% sd_mat
  return(us_mat)
}

# Simulate repeated measures outcomes based on the provided covariates
# data.frame and repeated measures covariance matrix. These outcomes are
# simulated according to a linear model with Gaussian errors. The linear model
# coefficients associated with each baseline covariate, visit, and visit x
# treatment interaction are set through their respective arguments. For
# generative modeling purposes, visit is treated as a discrete number,
# permitting the specification of a single coefficient for all visit x treatment
# interaction effects.
generate_outcomes <- function(
    covars_df,
    cov_mat,
    intercept = 5,
    base_bcva_coef = 1,
    strata_2_coef = -1,
    strata_3_coef = 1,
    trt_coef = 1,
    visit_coef = 0.25,
    trt_visit_coef = 0.25) {
  # construct the model matrix
  model_mat <- model.matrix(
    ~ base_bcva + strata + trt * visit_num,
    data = covars_df
  )

  # generate the bvca outcomes
  n_visits <- nrow(cov_mat)
  n_obs <- nrow(covars_df) / n_visits
  effect_coefs <- c(
    intercept, base_bcva_coef, strata_2_coef, strata_3_coef,
    trt_coef, visit_coef, trt_visit_coef
  )
  as.vector(model_mat %*% effect_coefs +
    as.vector(t(MASS::mvrnorm(n_obs, rep(0, n_visits), cov_mat))))
}

# Simulates intermittent dropout events as a function of measured baseline
# covariates and visit number. This ensures that observations are missing at
# random. The baseline covariates data.frame must be provided, as must the
# desired level of missingness. A long data.frame with missing observations is
# returned.
missing_at_random <- function(covars_df, type) {
  # compute missingness probabilities
  if (type == "low") {
    prob_miss <- plogis(
      -(5 - 0.01 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.25 * covars_df$visit_num)
    )
  } else if (type == "high") {
    prob_miss <- plogis(
      -(5 - 0.01 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.4 * covars_df$visit_num)
    )
  } else if (type == "extreme") {
    prob_miss <- plogis(
      -(5 - 0.02 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.5 * covars_df$visit_num -
        covars_df$trt)
    )
  }

  # generate vector of missingness indicators
  missing_ind <- rbinom(nrow(covars_df), 1, prob_miss)

  # remove indicated visits
  covars_df[missing_ind == 0, ]
}
