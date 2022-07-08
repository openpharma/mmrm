# h_mmrm_tmb_control ----

test_that("h_mmrm_tmb_control works as expected", {
  result <- h_mmrm_tmb_control(
    optimizer = stats::optim,
    optimizer_args = list(method = "L-BFGS-B")
  )
  expected <- structure(
    list(
      optimizer = stats::optim,
      optimizer_args = list(method = "L-BFGS-B"),
      optimizer_control = list()
    ),
    class = "mmrm_tmb_control"
  )
  expect_identical(result, expected)
})

# h_mmrm_tmb_formula_parts ----

test_that("h_mmrm_tmb_formula_parts works as expected", {
  result <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  ))
  expected <- structure(
    list(
      formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
      model_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + ARMCD:AVISIT,
      full_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + USUBJID + ARMCD:AVISIT,
      cov_type = "us",
      visit_var = "AVISIT",
      subject_var = "USUBJID"
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_formula_parts works without covariates", {
  result <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ ar1(AVISIT | USUBJID)
  ))
  expected <- structure(
    list(
      formula = FEV1 ~ ar1(AVISIT | USUBJID),
      model_formula = FEV1 ~ 1,
      full_formula = FEV1 ~ USUBJID + AVISIT,
      cov_type = "ar1",
      visit_var = "AVISIT",
      subject_var = "USUBJID"
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_formula_parts works as expected for antedependence", {
  result <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID)
  ))
  expected <- structure(
    list(
      formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID),
      model_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + ARMCD:AVISIT,
      full_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + USUBJID + ARMCD:AVISIT,
      cov_type = "ad",
      visit_var = "AVISIT",
      subject_var = "USUBJID"
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result, expected)
})

# h_mmrm_tmb_data ----

test_that("h_mmrm_tmb_data works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  result <- expect_silent(h_mmrm_tmb_data(formula_parts, fev_data, reml = FALSE))
  expect_class(result, "mmrm_tmb_data")
  expect_named(
    result,
    c(
      "full_frame", "x_matrix", "y_vector", "visits_zero_inds", "n_visits", "n_subjects",
      "subject_zero_inds", "subject_n_visits", "cov_type", "reml"
    )
  )
  expect_matrix(result$x_matrix, nrows = 537, ncols = 3, any.missing = FALSE)
  expect_numeric(result$y_vector, len = 537, any.missing = FALSE)
  expect_integer(result$visits_zero_inds, len = 537, lower = 0, upper = 3, any.missing = FALSE)
  expect_identical(result$n_visits, 4L) # 4 visits.
  expect_integer(result$subject_zero_inds, len = 197, unique = TRUE, sorted = TRUE, any.missing = FALSE)
  expect_identical(result$cov_type, "us") # unstructured.
  expect_identical(result$reml, 0L) # ML.
})

test_that("h_mmrm_tmb_data works also for character ID variable", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  dat <- fev_data
  dat$USUBJID <- as.character(dat$USUBJID) # nolint
  result <- expect_silent(h_mmrm_tmb_data(formula_parts, dat, reml = FALSE))
  expected <- expect_silent(h_mmrm_tmb_data(formula_parts, fev_data, reml = FALSE))
  expect_identical(result, expected)
})

# h_mmrm_tmb_parameters ----

test_that("h_mmrm_tmb_parameters works as expected without start values", {
  formula <- FEV1 ~ SEX + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 10))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with start values", {
  formula <- FEV1 ~ SEX + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  start <- 1:10
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = start))
  expected <- list(theta = start)
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with antedependence", {
  formula <- FEV1 ~ SEX + ad(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 7)) # 2 * 4 - 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with Toeplitz", {
  formula <- FEV1 ~ SEX + toep(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 7)) # 2 * 4 - 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with autoregressive", {
  formula <- FEV1 ~ SEX + ar1(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 2))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with heterogeneous autoregressive", {
  formula <- FEV1 ~ SEX + ar1h(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 5)) # 4 + 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with compound symmetry", {
  formula <- FEV1 ~ SEX + cs(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 2))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with heterogeneous compound symmetry", {
  formula <- FEV1 ~ SEX + csh(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = TRUE)
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 5)) # 4 + 1 parameters.
  expect_identical(result, expected)
})

# h_mmrm_tmb_assert_start ----

test_that("h_mmrm_tmb_assert_start passes as expected for sane start values", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) par,
    par = 1:2
  )
  result <- expect_silent(h_mmrm_tmb_assert_start(tmb_object))
  expect_null(result)
})

test_that("h_mmrm_tmb_assert_start fails as expected for NaN objective function at start", {
  tmb_object <- list(
    fn = function(par) NaN,
    gr = function(par) par,
    par = 1:2
  )
  expect_error(
    h_mmrm_tmb_assert_start(tmb_object),
    "negative log-likelihood is NaN at starting parameter values"
  )
})

test_that("h_mmrm_tmb_assert_start fails as expected for NaN gradient function at start", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) c(NaN, 0.5),
    par = 1:2
  )
  expect_error(
    h_mmrm_tmb_assert_start(tmb_object),
    "some elements of gradient are NaN at starting parameter values"
  )
})

# h_mmrm_tmb_assert_opt ----

test_that("h_mmrm_tmb_assert_opt passes as expected for sane optimization result", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) par * 2,
    he = function(par) tcrossprod(par) + diag(1, 5, 5),
    par = 5:1
  )
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 0,
    message = NULL
  )
  result <- expect_silent(h_mmrm_tmb_assert_opt(tmb_object, tmb_opt))
  expect_null(result, expected)
})

test_that("h_mmrm_tmb_assert_opt raises eigenvalue error as expected", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) par * 2,
    he = function(par) "bla",
    par = 5:1
  )
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 0,
    message = NULL
  )
  expect_error(
    h_mmrm_tmb_assert_opt(tmb_object, tmb_opt),
    "Model convergence problem: Cannot calculate hessian eigenvalues"
  )
})

test_that("h_mmrm_tmb_assert_opt warns for negative eigenvalues as expected", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) par * 2,
    he = function(par) -tcrossprod(par),
    par = 5:1
  )
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 0,
    message = NULL
  )
  expect_warning(
    h_mmrm_tmb_assert_opt(tmb_object, tmb_opt),
    "Model convergence problem: hessian has negative or very small eigenvalues"
  )
})

test_that("h_mmrm_tmb_assert_opt warns if convergence code signals non-convergence", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) par * 2,
    he = function(par) tcrossprod(par) + diag(1, 5, 5),
    par = 5:1
  )
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 1,
    message = "something ugly"
  )
  expect_warning(
    h_mmrm_tmb_assert_opt(tmb_object, tmb_opt),
    "Model convergence problem: something ugly."
  )
})

# h_mmrm_tmb_fit ----

test_that("h_mmrm_tmb_fit works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = FALSE)
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL)
  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_opt <- with(
    tmb_object,
    do.call(
      what = stats::nlminb,
      args = list(par, fn, gr)
    )
  )
  result <- expect_silent(h_mmrm_tmb_fit(tmb_object, tmb_opt, fev_data, formula_parts, tmb_data))
  expect_class(result, "mmrm_tmb")
  expect_named(result, c(
    "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
    "neg_log_lik", "formula_parts", "data", "reml", "opt_details", "tmb_object",
    "tmb_data"
  ))
  expect_identical(rownames(result$cov), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_identical(colnames(result$cov), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_named(result$beta_est, colnames(tmb_data$x_matrix))
  expect_identical(rownames(result$beta_vcov), colnames(tmb_data$x_matrix))
  expect_identical(colnames(result$beta_vcov), colnames(tmb_data$x_matrix))
  expect_matrix(result$theta_vcov, nrows = length(result$theta_est), ncols = length(result$theta_est))
  expect_number(result$neg_log_lik)
  expect_list(result$formula_parts)
  expect_data_frame(result$data)
  expect_false(result$reml)
  expect_list(result$opt_details)
  expect_list(result$tmb_object)
  expect_class(result$tmb_data, "mmrm_tmb_data")
})

test_that("h_mmrm_tmb_fit errors when an invalid covariance type is used", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)

  tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = FALSE)
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL)

  tmb_data$cov_type <- "gaaah"
  expect_error(regex = c("Unknown covariance type 'gaaah'"), {
    tmb_object <- TMB::MakeADFun(
      data = tmb_data,
      parameters = tmb_parameters,
      hessian = TRUE,
      DLL = "mmrm",
      silent = TRUE
    )
  })
})

# h_mmrm_tmb ----

## unstructured ----

test_that("h_mmrm_tmb works as expected in a simple model without covariates and ML", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(h_mmrm_tmb(formula, data, reml = FALSE))
  expect_class(result, "mmrm_tmb")
  expect_list(result)
  expect_named(
    result,
    c(
      "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
      "neg_log_lik", "formula_parts", "data", "reml", "opt_details", "tmb_object",
      "tmb_data", "call"
    )
  )
  # See design/SAS/sas_log_simple.txt for the source of numbers.
  expect_equal(2 * result$neg_log_lik, 3700.68707570)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3499, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.8367, tolerance = 1e-4)
  result_cov_tri <- result$cov[lower.tri(result$cov)]
  expected_cov_tri <- c(49.8121, 2.6246, -40.5833, 4.8398, -8.6607, 22.9163)
  expect_equal(result_cov_tri, expected_cov_tri, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works as expected in a simple model without covariates and REML", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(h_mmrm_tmb(formula, data, reml = TRUE))
  expect_class(result, "mmrm_tmb")
  expect_list(result)
  expect_named(
    result,
    c(
      "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
      "neg_log_lik", "formula_parts", "data", "reml", "opt_details", "tmb_object",
      "tmb_data", "call"
    )
  )
  # See design/SAS/sas_log_simple_reml.txt for the source of numbers.
  expect_equal(2 * result$neg_log_lik, 3700.94648570)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3509, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.8338, tolerance = 1e-4)
  result_cov_tri <- result$cov[lower.tri(result$cov)]
  expected_cov_tri <- c(49.8999, 2.7459, -40.4566, 4.9722, -8.5335, 23.0555)
  expect_equal(result_cov_tri, expected_cov_tri, tolerance = 1e-3)
})

## ante-dependence ----

test_that("h_mmrm_tmb works with ante-dependence covariance structure and ML", {
  formula <- FEV1 ~ ad(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(h_mmrm_tmb(formula, data, reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_antedependence_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3713.24501787)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3519, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.9019, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(114.78, 44.5191, 26.7673, 158.33))),
    map_to_theta(c(0.7104, 0.09992, 0.3650))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with ante-dependence covariance structure and REML", {
  formula <- FEV1 ~ ad(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(h_mmrm_tmb(formula, data, reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_antedependence_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3713.49317786)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3529, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.9009, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(114.89, 44.6313, 26.8922, 158.47))),
    map_to_theta(c(0.7106, 0.1033, 0.3657))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

## toeplitz ----

test_that("h_mmrm_tmb works with toeplitz covariance structure and ML", {
  formula <- FEV1 ~ toep(AVISIT | USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(h_mmrm_tmb(formula, data, reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_toeplitz_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3722.29178558)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3812, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.6769, tolerance = 1e-4)
  expected_var <- c(90.3084, 38.4717, 35.9728, 175.50)
  cor_mat <- VarCorr(result)
  result_var <- as.numeric(diag(cor_mat))
  expect_equal(result_var, expected_var, tolerance = 1e-4)
  result_low_tri <- cor_mat[lower.tri(cor_mat)]
  expected_low_tri <- c(25.2231, 2.4950, -39.4085, 15.9192, 3.5969, 34.0009)
  expect_equal(result_low_tri, expected_low_tri, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with ante-dependence covariance structure and REML", {
  formula <- FEV1 ~ toep(AVISIT | USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(h_mmrm_tmb(formula, data, reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_toeplitz_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3722.38018329)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3822, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.6726, tolerance = 1e-4)
  expected_var <- c(90.4041, 38.5927, 36.1518, 175.78)
  cor_mat <- VarCorr(result)
  result_var <- as.numeric(diag(cor_mat))
  expect_equal(result_var, expected_var, tolerance = 1e-4)
  result_low_tri <- cor_mat[lower.tri(cor_mat)]
  expected_low_tri <- c(25.3713, 2.6128, -39.2746, 16.0440, 3.7643, 34.2409)
  expect_equal(result_low_tri, expected_low_tri, tolerance = 1e-4)
})

## autoregressive ----

### homogeneous ----

test_that("h_mmrm_tmb works with ar1 covariance structure and ML", {
  formula <- FEV1 ~ ar1(AVISIT | USUBJID)
  result <- h_mmrm_tmb(formula, fev_data, reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3875.95352406)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.5000, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.3252, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(88.7088)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.4249
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with ar1 covariance structure and REML", {
  formula <- FEV1 ~ ar1(AVISIT | USUBJID)
  result <- h_mmrm_tmb(formula, fev_data, reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3875.49945459)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.5013, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.3255, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(88.9866)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.4272
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

### heterogeneous ----

test_that("h_mmrm_tmb works with ar1h covariance structure and ML", {
  formula <- FEV1 ~ ar1h(AVISIT | USUBJID)
  result <- h_mmrm_tmb(formula, fev_data, reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1h_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3739.94416707)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3936, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.7017, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(92.2283, 38.3927, 35.0194, 176.08))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.4113
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with ar1h covariance structure and REML", {
  formula <- FEV1 ~ ar1h(AVISIT | USUBJID)
  result <- h_mmrm_tmb(formula, fev_data, reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1h_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3739.96835012)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3947, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.6955, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(92.3009, 38.5178, 35.2148, 176.37))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.4130
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

## compound symmetry ----

### homogeneous ----

test_that("h_mmrm_tmb works with cs covariance structure and ML", {
  formula <- FEV1 ~ cs(AVISIT | USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(h_mmrm_tmb(formula, fev_data, reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_cs_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3918.12214544)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4273, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 42.2897, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(86.7143)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.06596
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

test_that("h_mmrm_tmb works with cs covariance structure and REML", {
  formula <- FEV1 ~ cs(AVISIT | USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(h_mmrm_tmb(formula, fev_data, reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_cs_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3917.98183508)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4285, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.2896, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(86.8968)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.06783
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

### heterogeneous ----

test_that("h_mmrm_tmb works with csh covariance structure and ML", {
  formula <- FEV1 ~ csh(AVISIT | USUBJID)
  result <- h_mmrm_tmb(formula, fev_data, reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_csh_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3784.63158043)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3566, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.0018, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(104.54, 38.5253, 31.1656, 178.62))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.1566
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with csh covariance structure and REML", {
  formula <- FEV1 ~ csh(AVISIT | USUBJID)
  result <- h_mmrm_tmb(formula, fev_data, reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_csh_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3784.85341599)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3575, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.9964, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(104.61, 38.6137, 31.3143, 178.91))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.1582
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

## misc ----

test_that("h_mmrm_tmb also works with character ID variable", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  data <- fev_data
  data$USUBJID <- as.character(data$USUBJID) # nolint
  result <- expect_silent(h_mmrm_tmb(formula, fev_data, reml = TRUE))
  expected <- expect_silent(h_mmrm_tmb(formula, data, reml = TRUE))
  expect_identical(result$beta_est, expected$beta_est)
})

test_that("h_mmrm_tmb saves data name in call element as expected", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  fit <- h_mmrm_tmb(formula, fev_data)
  saved_call <- fit$call
  expect_class(saved_call, "call")
  expect_identical(saved_call$data, "fev_data")
})

test_that("h_mmrm_tmb works even when timepoint variable has unused factor levels", {
  # Create a data set where one visit level only has NA in the data.
  tmp_data <- fev_data
  tmp_data$FEV1_BL[1] <- tmp_data$FEV1[1] <- NA # nolint
  tmp_data$AVISIT <- as.character(tmp_data$AVISIT) # nolint
  tmp_data$AVISIT[1] <- "SCREENING" # nolint
  tmp_data$AVISIT <- as.factor(tmp_data$AVISIT) # nolint

  result <- expect_silent(h_mmrm_tmb(
    FEV1 ~ FEV1_BL + RACE + us(AVISIT | USUBJID),
    data = tmp_data
  ))
  expect_class(result, "mmrm_tmb")
  expect_identical(
    rownames(VarCorr(result)),
    c("VIS1", "VIS2", "VIS3", "VIS4")
  )
})
