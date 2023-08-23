library(stringr)
library(MASS)
library(clusterGeneration)

# Helper function for generating covariates.
generate_covariates <- function(n_obs, n_visits = 10) {
  # Participant ID.
  participant <- seq_len(n_obs)

  # Baseline best corrected visual acuity score.
  base_bcva <- rnorm(n = n_obs, mean = 75, sd = 10)

  # Stratification factor.
  strata <- as.vector(
    c(1, 2, 3) %*% rmultinom(n = n_obs, 1, prob = c(0.3, 0.3, 0.4))
  )

  # Treatment indicator.
  trt <- rbinom(n = n_obs, size = 1, prob = 0.5)

  # Visit number.
  visit_num <- rep(seq_len(n_visits), n_obs)

  # Assemble into a covariates data frame.
  data.frame(
    participant = rep(participant, each = n_visits),
    base_bcva = rep(base_bcva, each = n_visits),
    strata = as.factor(rep(strata, each = n_visits)),
    trt = rep(trt, each = n_visits),
    visit_num
  )
}

# Helper function for randomly generating unstructured covariance matrix.
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

# Helper function for generating BCVA data.
generate_outcomes <- function(covars_df,
                              cov_mat,
                              intercept = 5,
                              base_bcva_coef = 1,
                              strata_2_coef = -1,
                              strata_3_coef = 1,
                              trt_coef = 1,
                              visit_coef = 0.25,
                              trt_visit_coef = 0.25) {
  # Construct the model matrix.
  model_mat <- model.matrix(
    ~ base_bcva + strata + trt * visit_num,
    data = covars_df
  )

  # Generate the bvca outcomes.
  n_visits <- nrow(cov_mat)
  n_obs <- nrow(covars_df) / n_visits
  effect_coefs <- c(
    intercept, base_bcva_coef, strata_2_coef, strata_3_coef,
    trt_coef, visit_coef, trt_visit_coef
  )
  as.vector(model_mat %*% effect_coefs + t(MASS::mvrnorm(n_obs, rep(0, n_visits), cov_mat)))
}

# MAR helper function.
missing_at_random <- function(covars_df, type) {
  # Compute missingness probabilities.
  lin_pred <- function(coef_visit, coef_trt) {
    -(5 - 0.01 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) + coef_visit * covars_df$visit_num +
        coef_trt * (covars_df$trt == 0))
  }
  prob_miss <- if (type == "none") {
    0
  } else if (type == "mild") {
    plogis(lin_pred(-0.3, -0.2))
  } else if (type == "moderate") {
    plogis(lin_pred(-0.4, -0.5))
  } else if (type == "high") {
    plogis(lin_pred(-0.5, -1))
  }

  # Generate vector of missingness indicators.
  missing_ind <- rbinom(nrow(covars_df), 1, prob_miss)

  # Only keep non-missing visits.
  covars_df[missing_ind == 0, ]
}

# BCVA data-generating process.
rct_dgp_fun <- function(n_obs = 1000,
                        outcome_covar_mat = compute_unstructured_matrix(),
                        trt_coef = 0.25,
                        visit_coef = 0.25,
                        trt_visit_coef = 0.25,
                        missing_type = "moderate") {
  # Generate the covariates.
  covars_df <- generate_covariates(
    n_obs = n_obs, n_visits = nrow(outcome_covar_mat)
  )

  # Generate the outcomes.
  bcva_out <- generate_outcomes(
    covars_df = covars_df,
    cov_mat = outcome_covar_mat,
    trt_coef = trt_coef,
    visit_coef = visit_coef,
    trt_visit_coef = trt_visit_coef
  )

  # Compute the change in BCVA.
  bcva_change <- bcva_out - covars_df$base_bcva

  # Assemble into a data.frame.
  df <- data.frame(
    participant = covars_df$participant,
    bcva_change = bcva_change,
    base_bcva = covars_df$base_bcva,
    strata = covars_df$strata,
    trt = covars_df$trt,
    visit_num = covars_df$visit_num
  )

  # Delete observations at random.
  df <- missing_at_random(df, type = missing_type)

  # Format to resemble FEV dataset.
  df %>%
    transmute(
      USUBJID = factor(participant),
      VISITN = visit_num,
      AVISIT = paste0("VIS", str_pad(visit_num, width = 2, pad = "0")),
      AVISIT = factor(
        AVISIT,
        levels = paste0("VIS", str_pad(seq_len(10), width = 2, pad = "0"))
      ),
      ARMCD = ifelse(trt == 1, "TRT", "CTL"),
      RACE = ifelse(strata == 1, "Black",
        ifelse(strata == 2, "Asian", "White")
      ),
      BCVA_BL = base_bcva,
      BCVA_CHG = bcva_change
    )
}
