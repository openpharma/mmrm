# fit_single_optimizer ----

test_that("fit_single_optimizer works as expected with defaults", {
  dat <- fev_data
  form <- FEV1 ~ ARMCD * RACE + us(AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat,
    weights = rep(1, nrow(dat))
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "L-BFGS-B")
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer works as expected with nlminb optimizer but no starting values", {
  dat <- fev_data
  form <- FEV1 ~ SEX + us(AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat,
    weights = rep(1, nrow(dat)),
    optimizer = "nlminb"
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "nlminb")
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer works as expected with optimizer inputted but no starting values", {
  dat <- fev_data
  form <- FEV1 ~ SEX + us(AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat,
    weights = rep(1, nrow(dat)),
    optimizer = "BFGS"
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "BFGS")
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer works as expected with starting values and optimizer inputted", {
  dat <- fev_data
  form <- FEV1 ~ RACE + ARMCD + us(AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat,
    weights = rep(1, nrow(dat)),
    optimizer = "BFGS",
    start = 1:10
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "BFGS")
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer gives error messages", {
  formula <- FEV1 ~ bla
  expect_error(
    fit_mmrm(formula, fev_data),
    paste(
      "Covariance structure must be specified in formula.",
      "Possible covariance structures include: us, toep, toeph, ar1, ar1h, ad, adh, cs, csh"
    ),
    fixed = TRUE
  )
  expect_error(
    fit_single_optimizer(formula, fev_data, weights = rep(1, nrow(fev_data))),
    paste(
      "Covariance structure must be specified in formula.",
      "Possible covariance structures include: us, toep, toeph, ar1, ar1h, ad, adh, cs, csh"
    ),
    fixed = TRUE
  )
})

test_that("fit_single_optimizer is stable to extreme scaling with defaults", {
  # We generate an example data set with two variables on very different scales.
  # After rescaling to sd = 1 both would have unit effect on the outcome.
  scaling_factor <- 1e6

  set.seed(42L)
  id <- factor(rep(1:50, each = 2))
  visit <- factor(rep(c(1, 2), 50))
  x_large <- rep(rnorm(50, sd = sqrt(scaling_factor)), each = 2)
  x_small <- rep(rnorm(50, sd = 1 / sqrt(scaling_factor)), each = 2)
  y <- x_large / sqrt(scaling_factor) + sqrt(scaling_factor) * x_small + rnorm(100, sd = .5) # add some noise
  dat <- data.frame(id = id, x_large = x_large, x_small = x_small, visit = visit, y = y)

  result <- fit_single_optimizer(
    formula = y ~ x_large + x_small + us(visit | id),
    data = dat,
    weights = rep(1, nrow(dat))
  )

  # Compute relative error on both coefficients and check that it is small.
  beta_est <- coef(result)[c("x_large", "x_small")]
  rel_err <- c(beta_est - c(1 / sqrt(scaling_factor), sqrt(scaling_factor))) /
    c(1 / sqrt(scaling_factor), sqrt(scaling_factor))
  expect_true(attr(result, "converged"))
  expect_true(all(abs(rel_err) <= 0.05))
})

test_that("fit_single_optimizer catches convergence warning as expected", {
  dat <- data.frame(
    FEV1 = c(1, 2, 3, 4, 5),
    AVISIT = factor(c("V1", "V1", "V2", "V3", "V4")),
    USUBJID = c("A", "B", "A", "C", "A")
  )
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  result <- expect_silent(fit_single_optimizer(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = dat,
    weights = rep(1, nrow(dat)),
    optimizer = "L-BFGS-B"
  ))
  expect_match(attr(result, "warnings"), regexp = "Model convergence problem")
  expect_false(attr(result, "converged"))
  expect_class(component(result, "theta_vcov"), "try-error")
})

# h_summarize_all_fits ----

test_that("h_summarize_all_fits works as expected", {
  mod_fit <- fit_single_optimizer(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "nlminb"
  )
  mod_fit2 <- fit_single_optimizer(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "L-BFGS-B"
  )
  all_fits <- list(mod_fit, mod_fit2)
  result <- expect_silent(h_summarize_all_fits(all_fits))
  expected <- list(
    warnings = list(NULL, NULL),
    messages = list(NULL, NULL),
    log_liks = c(-1693.22493558573, -1693.22493812251),
    converged = c(TRUE, TRUE)
  )
  expect_equal(result, expected)
})

test_that("h_summarize_all_fits works when some list elements are try-error objects", {
  mod_fit <- fit_single_optimizer(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "nlminb"
  )
  mod_fit2 <- try(stop("bla"), silent = TRUE)
  mod_fit3 <- fit_single_optimizer(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "L-BFGS-B"
  )
  all_fits <- list(mod_fit, mod_fit2, mod_fit3)
  result <- expect_silent(h_summarize_all_fits(all_fits))
  expected <- list(
    warnings = list(NULL, "Error in try(stop(\"bla\"), silent = TRUE) : bla\n", NULL),
    messages = list(NULL, NULL, NULL),
    log_liks = c(-1693.22493558573, NA, -1693.22493812251),
    converged = c(TRUE, FALSE, TRUE)
  )
  expect_equal(result, expected)
})

# refit_multiple_optimizers ----

test_that("refit_multiple_optimizers works as expected with default arguments", {
  fit <- fit_single_optimizer(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "nlminb"
  )

  # Mock here that it did not converge.
  attr(fit, "converged") <- FALSE
  fit$neg_log_lik <- fit$neg_log_lik + 10

  result <- expect_silent(refit_multiple_optimizers(fit = fit))
  expect_class(result, "mmrm_fit")

  expect_true(attr(result, "converged"))
  expect_false(identical("nlminb", attr(result, "optimizer")))
  expect_true(logLik(result) > logLik(fit))
})

test_that("refit_multiple_optimizers works with parallel computations and selected optimizers", {
  skip_on_cran()

  fit <- fit_single_optimizer(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "nlminb"
  )

  # Mock here that it did not converge.
  attr(fit, "converged") <- FALSE
  fit$neg_log_lik <- fit$neg_log_lik + 10

  result <- expect_silent(refit_multiple_optimizers(
    fit = fit,
    n_cores = free_cores(),
    optimizers = c("BFGS", "CG")
  ))
  expect_class(result, "mmrm_fit")

  expect_true(attr(result, "converged"))
  expect_subset(attr(result, "optimizer"), choices = c("BFGS", "CG"))
  expect_true(logLik(result) > logLik(fit))
})

# mmrm ----

## unstructured ----

test_that("mmrm works as expected for unstructured", {
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  result <- expect_silent(mmrm(formula, fev_data, reml = FALSE))
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_list(result$jac_list, types = "matrix")
  expect_class(result$call, "call")
  expect_false(result$reml)
})

test_that("mmrm works as expected for antedependence", {
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID)
  result <- expect_silent(mmrm(formula, fev_data, reml = TRUE))
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_list(result$jac_list, types = "matrix")
  expect_class(result$call, "call")
  expect_true(result$reml)
})

test_that("mmrm works as expected for toeplitz", {
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + toep(AVISIT | USUBJID)
  result <- expect_silent(mmrm(formula, fev_data, reml = TRUE))
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_list(result$jac_list, types = "matrix")
  expect_class(result$call, "call")
  expect_true(result$reml)
})

## general ----

test_that("mmrm falls back to other optimizers if default does not work", {
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  data_small <- fev_data[1:50, ]
  # Default does not work.
  expect_error(
    mmrm(formula, data_small, optimizer = "L-BFGS-B"),
    "Model convergence problem"
  )
  # But another one works.
  # Note: We disable parallel processing here to comply with CRAN checks.
  result <- expect_silent(mmrm(formula, data_small, n_cores = 1L))
  expect_true(attr(result, "converged"))
  expect_false(identical(attr(result, "optimizer"), "L-BFGS-B"))
})

test_that("mmrm fails if no optimizer works", {
  skip_on_ci()
  skip_on_cran()
  # We skip this test on CI and CRAN since it can be flaky on different platforms.

  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  data_small <- fev_data[1:30, ]
  # Note: Here we are using parallel computations.
  expect_error(
    mmrm(formula, data_small, reml = FALSE),
    "No optimizer led to a successful model fit"
  )
})

test_that("mmrm works for rank deficient original design matrix by default", {
  formula <- FEV1 ~ RACE + SEX + SEX2 + ARMCD * AVISIT + us(AVISIT | USUBJID)
  dat <- fev_data
  dat$SEX2 <- dat$SEX # nolint
  result <- expect_silent(mmrm(formula, dat))
  expect_true(attr(result, "converged"))
})

test_that("mmrm works for specific small data example", {
  small_dat <- data.frame(
    FEV1 = c(1, 2, 3, 4, 5, 6),
    AVISIT = factor(c("V1", "V1", "V2", "V3", "V3", "V4")),
    USUBJID = c("A", "B", "A", "C", "D", "A")
  )
  vars <- list(
    response = "FEV1",
    id = "USUBJID",
    visit = "AVISIT"
  )
  fit <- expect_silent(mmrm::mmrm(
    formula = FEV1 ~ AVISIT + ar1(AVISIT | USUBJID),
    data = small_dat
  ))
  expect_true(attr(fit, "converged"))
})
