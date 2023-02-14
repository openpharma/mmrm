generate_covariates <- function(n_obs, n_visits = 10) {

  # baseline best corrected visual acuity score
  base_bcva <- rnorm(n = n_obs, mean = 59, sd = 3)

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
    base_bcva = rep(base_bcva, each = n_visits),
    strata = rep(strata, each = n_visits),
    trt = rep(trt, each = n_visits),
    visit_num
  )
}

compute_csh_matrix <- function(vars, corr) {
  csh_mat <- tcrossprod(sqrt(vars), sqrt(vars)) * corr
  diag(csh_mat) <- vars
  return(csh_mat)
}

compute_unstructured_matrix <- function(
  vars = seq(from = 1, by = 0.5, length.out = 10)
) {
  n_visits <- length(vars)
  corr_mat <- abs(cov2cor(
    clusterGeneration::genPositiveDefMat(dim = n_visits)$Sigma
  ))
  sd_mat <- diag(sqrt(vars))
  us_mat <- sd_mat %*% corr_mat %*% sd_mat
  return(us_mat)
}
