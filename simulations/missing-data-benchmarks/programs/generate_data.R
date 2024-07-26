generate_data <- function(n_sample_size, n_rep, n_visits, fixed = fixed_effect, missing_level = "no", trt_visit_coef = 0, ...) {
  n_obs <- n_sample_size * n_rep
  # participant ID
  participant <- seq_len(n_obs)
  group <- ceiling(participant / n_sample_size)
  # baseline best corrected visual acuity score
  bcva_bl <- rnorm(n = n_obs, mean = 75, sd = 10)
  # statification factor
  strata <- sample(1:3, size = n_obs, replace = TRUE, prob = c(0.3, 0.3, 0.4))
  # treatment indicator
  trt <- rbinom(n = n_obs, size = 1, prob = 0.5)
  # assemble into a covariates data.frame
  ret <- data.frame(
    id = rep(participant, each = n_visits),
    bcva_bl = rep(bcva_bl, each = n_visits),
    strata = as.factor(rep(strata, each = n_visits)),
    trt = rep(trt, each = n_visits),
    group = rep(group, each = n_visits),
    visit = rep(seq_len(n_visits), n_obs)
  )
  chgfix <- fixed_effect(ret, trt_visit_coef = trt_visit_coef, ...)
  error_us <- as.vector(t(MASS::mvrnorm(n_obs, rep(0, n_visits), compute_unstructured_matrix(n_visits))))
  error_toep <- as.vector(t(MASS::mvrnorm(n_obs, rep(0, n_visits), compute_csh_matrix(n_visits))))
  error_csh <- as.vector(t(MASS::mvrnorm(n_obs, rep(0, n_visits), compute_topelitz_matrix(n_visits))))
  ret$chgus <- chgfix + error_us
  ret$chgtoep <- chgfix + error_toep
  ret$chgcsh <- chgfix + error_csh
  ret <- missing_at_random(ret, missing_level)
  ret$visit <- factor(ret$visit, levels = 1:10)
  ret$strata <- factor(ret$strata, levels = 1:3)
  ret
}

fixed_effect <- function(
    df, intercept = 5,
    base_bcva_coef = 0,
    strata_2_coef = -1,
    strata_3_coef = 1,
    trt_coef = 0,
    visit_coef = 0.25,
    trt_visit_coef = 0, ...) {
  intercept + base_bcva_coef * df$bcva_bl +
    strata_2_coef * (df$strata == 2) +
    strata_3_coef * (df$strata == 3) +
    trt_coef * df$trt +
    visit_coef * df$visit +
    trt_visit_coef * df$trt * df$visit
}

# Randomly generate a heterogeneous compound symmetry covariance matix. The
# variances and correlation parameters are set using the vars and corr
# arguments, respectively.
compute_csh_matrix <- function(n_visits, corr = 0.25) {
  vars <- seq(from = 1, by = 0.5, length.out = n_visits)
  csh_mat <- tcrossprod(sqrt(vars), sqrt(vars)) * corr
  diag(csh_mat) <- vars
  return(csh_mat)
}

# Randomly generate a unstructured covariance matix. The variances are set using
# the vars argument.
compute_unstructured_matrix <- function(n_visits) {
  vars <- seq(from = 1, by = 0.5, length.out = n_visits)
  corr_mat <- abs(cov2cor(
    clusterGeneration::genPositiveDefMat(dim = n_visits)$Sigma
  ))
  sd_mat <- diag(sqrt(vars))
  us_mat <- sd_mat %*% corr_mat %*% sd_mat
  return(us_mat)
}

compute_topelitz_matrix <- function(n_visits) {
  toeplitz(c(1, 0.5, 0.25, 0.125, rep(0, n_visits - 4)))
}

# Simulates intermittent dropout events as a function of measured baseline
# covariates and visit number. This ensures that observations are missing at
# random. The baseline covariates data.frame must be provided, as must the
# desired level of missingness. A long data.frame with missing observations is
# returned.
missing_at_random <- function(covars_df, type, ...) {
  # compute missingness probabilities
  if (type == "no") {
    return(covars_df)
  }
  if (type == "low") {
    prob_miss <- plogis(
      -(5 - 0.01 * covars_df$bcva_bl + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.25 * covars_df$visit)
    )
  } else if (type == "high") {
    prob_miss <- plogis(
      -(5 - 0.01 * covars_df$bcva_bl + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.4 * covars_df$visit)
    )
  } else if (type == "extreme") {
    prob_miss <- plogis(
      -(5 - 0.02 * covars_df$bcva_bl + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.5 * covars_df$visit)
    )
  }

  # generate vector of missingness indicators
  missing_ind <- rbinom(nrow(covars_df), 1, prob_miss)

  # remove indicated visits
  covars_df[missing_ind == 0, ]
}
