test_that(paste(
  "generate_covariates with 5 and 10 visits produces a 50 observation",
  "data.frame"
), {
  expect_equal(nrow(generate_covariates(n_obs = 5, n_visits = 10)), 50)
})

test_that(paste(
  "generate_covariates produces data with approximately 50% treatment",
  "assignments"
), {
  library(dplyr)
  set.seed(151)
  est_trt_prop <- generate_covariates(n_obs = 1000) %>%
    filter(visit_num == 1) %>%
    summarize(mean(trt)) %>%
    pull(1)
  expect_equal(est_trt_prop, 0.5, tolerance = 0.1)
})

test_that(paste(
  "generate_covariates produces data with baseline BVCA values centered",
  "at 75"
), {
  library(dplyr)
  set.seed(5123)
  est_base_mean <- generate_covariates(n_obs = 1000) %>%
    filter(visit_num == 1) %>%
    summarize(mean(base_bcva)) %>%
    pull(1)
  expect_equal(est_base_mean, 75, tolerance = 0.5)
})

test_that(paste(
  "generate_covariates produces data with strata values 1, 2 and 3",
  "occurring with probabilities 0.3, 0.3 and 0.4, respectively"
), {
  library(dplyr)
  set.seed(61234)
  strata_values <- generate_covariates(n_obs = 1000) %>%
    filter(visit_num == 1) %>%
    pull(strata)
  expect_equal(mean(strata_values == 1), 0.3, tolerance = 0.1)
  expect_equal(mean(strata_values == 2), 0.3, tolerance = 0.1)
  expect_equal(mean(strata_values == 3), 0.4, tolerance = 0.1)
})

test_that(paste(
  "compute_csh_matrix with vars = c(1, 4, 9) and corr = 0.2 produces",
  "a heterogeneous compound symmetry matrix with variances equal to",
  "1, 4 and 9, and a correlation coefficient equal to 0.2"
), {
  true_matrix <- matrix(
    c(1, 0.4, 0.6,
      0.4, 4, 1.2,
      0.6, 1.2, 9),
    nrow = 3, ncol = 3
  )
  csh_matrix <- compute_csh_matrix(vars = c(1, 4, 9), corr = 0.2)
  expect_equal(csh_matrix, true_matrix)
})

test_that(paste(
  "compute_unstructured_matrix with",
  "vars = seq(from = 1, by = 0.5, length.out = 10) produces a PSD covariance",
  "matrix with variances equal toseq(from = 1, by = 0.5, length.out = 10)"
), {
  true_vars <- seq(from = 1, by = 0.5, length.out = 10)
  us_mat <- compute_unstructured_matrix(vars = true_vars)
  expect_equal(diag(us_mat), true_vars)
  expect_equal(sum(eigen(us_mat)$values > 0), length(true_vars))
})

test_that(paste(
  "passing a covariates dataframe with 10 observations and 5 visits produces",
  "a vector of 50 outcomes"
), {
  set.seed(510)
  covars_df <- generate_covariates(n_obs = 10, n_visits = 5)
  cov_mat <- compute_csh_matrix(vars = rep(1, 5), corr = 0.2)
  bcva_out <- generate_outcomes(covars_df, cov_mat)
  expect_equal(length(bcva_out), 50)
})
