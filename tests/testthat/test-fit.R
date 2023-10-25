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
  expect_identical(component(result, "optimizer"), "L-BFGS-B")
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
  expect_identical(component(result, "optimizer"), "nlminb")
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
  expect_identical(component(result, "optimizer"), "BFGS")
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
  expect_identical(component(result, "optimizer"), "BFGS")
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
      "Possible covariance structures include:",
      "us, toep, toeph, ar1, ar1h, ad, adh, cs, csh, sp_exp"
    ),
    fixed = TRUE
  )
  expect_error(
    fit_single_optimizer(formula, fev_data, weights = rep(1, nrow(fev_data))),
    paste(
      "Covariance structure must be specified in formula.",
      "Possible covariance structures include:",
      "us, toep, toeph, ar1, ar1h, ad, adh, cs, csh"
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

test_that("fit_single_optimizer deals correctly with unobserved visits message", {
  data_unobs <- fev_data[fev_data$AVISIT != "VIS3", ]
  result <- expect_silent(fit_single_optimizer(
    formula = FEV1 ~ RACE + ad(AVISIT | USUBJID),
    data = data_unobs,
    weights = rep(1, nrow(data_unobs))
  ))
  expect_identical(attr(result, "messages"), "In AVISIT there are dropped visits: VIS3")
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer fails if formula contains `.`", {
  expect_error(
    fit_single_optimizer(FEV1 ~ ., data = fev_data, weights = rep(1, 800)),
    "`.` is not allowed in mmrm models!"
  )
})

test_that("fit_single_optimizer signals non-convergence but does not fail for finite values error", {
  fake_optimizer <- function(par, fun, grad, ...) {
    stop("L-BFGS-B needs finite values of 'fn'")
  }
  result <- expect_silent(fit_single_optimizer(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    control = mmrm_control(
      optimizer_fun = fake_optimizer
    )
  ))
  expect_false(attr(result, "converged"))
  expect_identical(attr(result, "divergence"), "L-BFGS-B needs finite values of 'fn'")
})

test_that("fit_single_optimizer signals non-convergence but does not fail for NA/NaN Hessian error", {
  fake_optimizer <- function(par, fun, grad, ...) {
    stop("NA/NaN Hessian evaluation")
  }
  result <- expect_silent(fit_single_optimizer(
    formula = FEV1 ~ AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    control = mmrm_control(
      optimizer_fun = fake_optimizer
    )
  ))
  expect_false(attr(result, "converged"))
  expect_identical(attr(result, "divergence"), "NA/NaN Hessian evaluation")
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

  # NOTE:
  # Test currently fails on R-devel on Fedora with clang for the first optimizer
  # mmrm v0.1.5 @ r-devel-linux-x86_64-fedora-clang
  #   `actual$log_liks`: -2155.280410081641 -1693.224938122514
  #   `expected$log_liks`: -1693.224935585730 -1693.224938122510
  #
  # As this failure only appears on R-devel on Fedora with clang, we are
  # temporarily skipping this test on CRAN until we can reproduce the behavior.
  skip_on_cran()

  expected <- list(
    warnings = list(NULL, NULL),
    messages = list(NULL, NULL),
    log_liks = c(-1693.22493558573, -1693.22493812251),
    converged = c(TRUE, TRUE)
  )

  expect_equal(result, expected)
})

test_that("h_summarize_all_fits works when some list elements are try-error objects", {
  mod_fit <- get_mmrm()
  mod_fit2 <- try(stop("bla"), silent = TRUE)
  mod_fit3 <- get_mmrm()
  all_fits <- list(mod_fit, mod_fit2, mod_fit3)
  result <- expect_silent(h_summarize_all_fits(all_fits))
  expected <- list(
    warnings = list(NULL, "Error in try(stop(\"bla\"), silent = TRUE) : bla\n", NULL),
    messages = list(NULL, NULL, NULL),
    log_liks = c(-1693.225, NA, -1693.225),
    converged = c(TRUE, FALSE, TRUE)
  )
  expect_equal(result, expected, tolerance = 1e-3)
})

# refit_multiple_optimizers ----

test_that("refit_multiple_optimizers works as expected with default arguments", {
  fit <- fit_single_optimizer(
    formula = FEV1 ~ ar1(AVISIT | USUBJID),
    data = fev_data,
    weights = rep(1, nrow(fev_data)),
    optimizer = "nlminb"
  )
  assert_true(attr(fit, "converged"))

  # Mock here that it did not converge.
  attr(fit, "converged") <- FALSE
  fit$neg_log_lik <- fit$neg_log_lik + 10

  result <- expect_silent(refit_multiple_optimizers(fit = fit, optimizer = "nlminb"))
  expect_class(result, "mmrm_fit")

  expect_true(attr(result, "converged"))
  expect_identical("nlminb", component(result, "optimizer"))
  expect_true(logLik(result) > logLik(fit))
})

test_that("refit_multiple_optimizers works with parallel computations and selected optimizers", {
  skip_on_cran()
  skip_if(!isTRUE(parallel::detectCores() > 1), "unable to detect more than one core")

  has_parallelly <- length(find.package("parallelly", quiet = TRUE)) > 0
  n_cores <- if (has_parallelly) parallelly::availableCores(omit = 1) else 2

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
    n_cores = n_cores,
    optimizer = c("BFGS", "CG")
  ))
  expect_class(result, "mmrm_fit")

  expect_true(attr(result, "converged"))
  expect_subset(component(result, "optimizer"), choices = c("BFGS", "CG"))
  expect_true(logLik(result) > logLik(fit))
})

# mmrm ----

## convergence ----

test_that("mmrm works as expected for unstructured", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
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
  formula <- FEV1 ~ ar1(AVISIT | USUBJID)

  # Chosen optimizer does not work.
  fake_optimizer <- function(par, fun, grad, ...) {
    stop("NA/NaN Hessian evaluation")
  }
  expect_error(
    expect_warning(
      mmrm(formula, fev_data, optimizer_fun = fake_optimizer),
      "Divergence with optimizer"
    ),
    "Consider trying multiple or different optimizers"
  )

  # But another one works.
  # Note: We disable parallel processing here to comply with CRAN checks.
  expect_warning(
    result <- mmrm(
      formula, fev_data,
      n_cores = 1L,
      optimizer_fun = c(fake = fake_optimizer, h_get_optimizers("L-BFGS-B"))
    ),
    "Divergence with optimizer"
  )
  expect_true(attr(result, "converged"))
  expect_true(identical(component(result, "optimizer"), "L-BFGS-B"))
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
  formula <- FEV1 ~ SEX + SEX2 + ar1(AVISIT | USUBJID)
  dat <- fev_data
  dat$SEX2 <- dat$SEX # nolint
  result <- expect_silent(mmrm(formula, dat))
  expect_true(attr(result, "converged"))
  expect_true(is.na(coef(result)["SEX2Female"]))
})

test_that("mmrm works if data is not provided as argument", {
  result <- expect_silent(
    with(
      fev_data,
      mmrm(FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID))
    )
  )
  expect_true(attr(result, "converged"))
})

test_that("mmrm works if formula contains variables not in data", {
  set.seed(123L)
  y <- rnorm(800)
  wt <- exp(rnorm(800))
  result <- expect_silent(
    mmrm(y ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID), weights = wt, data = fev_data)
  )
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
  fit <- expect_silent(mmrm(
    formula = FEV1 ~ AVISIT + ar1(AVISIT | USUBJID),
    data = small_dat
  ))
  expect_true(attr(fit, "converged"))
})

test_that("mmrm works for custom optimizer", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    optimizer_fun = silly_optimizer,
    optimizer_args = list(value_add = 2, message = "this is wrong"),
    start = c(1, 2, 3, 4),
    method = "Kenward-Roger"
  )
  expect_identical(fit$theta_est, c(3, 4, 5, 6))
})

test_that("mmrm works for constructed control", {
  expect_silent(mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    control = mmrm_control(optimizer = c("BFGS", "CG"))
  ))
})

test_that("mmrm still works for deprecated 'automatic' optimizer", {
  expect_warning(
    mmrm(
      FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID),
      data = fev_data,
      reml = TRUE,
      optimizer = "automatic"
    ),
    "\"automatic\" optimizer was deprecated in mmrm 0.2.0.",
    fixed = TRUE
  )
})

test_that("mmrm works and gives message for data with unobserved visit levels", {
  data_unobs <- fev_data[fev_data$AVISIT != "VIS3", ]
  expect_message(
    result <- mmrm(
      FEV1 ~ RACE + ad(AVISIT | USUBJID),
      data = data_unobs
    ),
    "In AVISIT there are dropped visits: VIS3"
  )
  data_dropped <- droplevels(data_unobs)
  expected <- mmrm(
    FEV1 ~ RACE + ad(AVISIT | USUBJID),
    data = data_dropped
  )
  expect_identical(deviance(result), deviance(expected))
})

test_that("mmrm fails when using ... and control at the same time", {
  expect_error(
    mmrm(
      formula = FEV1 ~ us(AVISIT | USUBJID),
      data = fev_data,
      accept_singular = FALSE,
      control = mmrm_control(method = "Kenward-Roger")
    ),
    "Assertion on '!missing(control) && !missing(...)' failed",
    fixed = TRUE
  )
})

test_that("mmrm fails when using formula covariance with covariance argument", {
  expect_error(
    mmrm(
      formula = FEV1 ~ us(AVISIT | USUBJID),
      covariance = cov_struct("us", "AVISIT", "USUBJID"),
      data = fev_data,
    ),
    "Redundant covariance structure",
    fixed = TRUE
  )
})

test_that("mmrm works when using formula directly in covariance", {
  expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD,
      covariance = ~ us(AVISIT | USUBJID),
      data = fev_data,
    )
  )
})


test_that("mmrm works for different na.actions", {
  na_action <- getOption("na.action")
  on.exit(options(na.action = na_action))
  options(na.action = "na.omit")
  formula <- FEV1 ~ ARMCD + us(AVISIT | USUBJID)

  res1 <- expect_silent(mmrm(formula, fev_data))
  expect_class(res1, "mmrm")

  options(na.action = "na.pass")
  expect_warning(
    res2 <- mmrm(formula, fev_data),
    "na.action is always set to `na.omit` for `mmrm` fit!"
  )
  expect_class(res2, "mmrm")

  options(na.action = "na.fail")
  expect_warning(
    res3 <- mmrm(formula, fev_data),
    "na.action is always set to `na.omit` for `mmrm` fit!"
  )
  expect_class(res3, "mmrm")

  options(na.action = "na.exclude")
  expect_warning(
    res4 <- mmrm(formula, fev_data),
    "na.action is always set to `na.omit` for `mmrm` fit!"
  )
  expect_class(res4, "mmrm")
})

test_that("mmrm still works for a model that only contains an interaction term", {
  expect_silent(
    mmrm(
      FEV1 ~ ARMCD:SEX + ar1(AVISIT | SEX / USUBJID),
      data = fev_data,
      reml = TRUE
    )
  )
})

test_that("mmrm fails if formula contains `.`", {
  expect_error(
    mmrm(FEV1 ~ ., data = fev_data, weights = rep(1, 800)),
    "`.` is not allowed in mmrm models!"
  )
})

test_that("mmrm can proceed to second optimizer if first one has divergence error", {
  fake_optimizer <- function(par, fun, grad, ...) {
    stop("NA/NaN Hessian evaluation")
  }
  expect_warning(
    result <- mmrm(
      formula = FEV1 ~ ARMCD + us(AVISIT | USUBJID),
      data = fev_data,
      control = mmrm_control(optimizers = c(fake = fake_optimizer, h_get_optimizers()))
    ),
    "Divergence with optimizer fake due to problems"
  )
  expect_true(attr(result, "converged"))
})

test_that("mmrm behaves correctly when some of alternative optimizers have divergence errors", {
  fake_optimizer <- function(par, fun, grad, ...) {
    stop("NA/NaN Hessian evaluation")
  }
  normal_optimizers <- h_get_optimizers()
  expect_warning(
    result <- mmrm(
      formula = FEV1 ~ ARMCD + us(AVISIT | USUBJID),
      data = fev_data,
      control = mmrm_control(
        optimizers = c(fake = fake_optimizer, normal_optimizers[1:2], fake2 = fake_optimizer)
      )
    ),
    "Divergence with optimizer fake due to problems"
  )
  expect_true(attr(result, "converged"))
})

test_that("mmrm works as expected when the only provided optimizer fails with divergence error", {
  fake_optimizer <- function(par, fun, grad, ...) {
    stop("NA/NaN Hessian evaluation")
  }
  expect_error(
    expect_warning(
      result <- mmrm(
        formula = FEV1 ~ ARMCD + us(AVISIT | USUBJID),
        data = fev_data,
        optimizer_fun = fake_optimizer
      ),
      "Divergence with optimizer"
    ),
    "Consider trying multiple or different optimizers"
  )
})

## vcov and method combination ----

test_that("mmrm works for vcov: Asymptotic and method: Sattherthwaite", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Satterthwaite",
      vcov = "Asymptotic"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_list(result$jac_list, types = "matrix")
})

test_that("mmrm works for vcov: Asymptotic and method: Between-Within", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Between-Within",
      vcov = "Asymptotic"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
})

test_that("mmrm fails for vcov: Kenward-Roger and method: Sattherthwaite", {
  expect_error(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Satterthwaite",
      vcov = "Kenward-Roger"
    ),
    "Kenward-Roger degrees of freedom must work together with Kenward-Roger or Kenward-Roger-Linear covariance!"
  )
})

test_that("mmrm fails for vcov: Kenward-Roger-Linear and method: Sattherthwaite", {
  expect_error(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Satterthwaite",
      vcov = "Kenward-Roger-Linear"
    ),
    "Kenward-Roger degrees of freedom must work together with Kenward-Roger or Kenward-Roger-Linear covariance!"
  )
})

test_that("mmrm works for vcov: Empirical and method: Residual", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Residual",
      vcov = "Empirical"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_matrix(result$beta_vcov_adj)
})

test_that("mmrm works for vcov: Jackknife and method: Residual", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Residual",
      vcov = "Empirical-Jackknife"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_matrix(result$beta_vcov_adj)
})

test_that("mmrm works for vcov: Empirical and method: Satterthwaite", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Satterthwaite",
      vcov = "Empirical"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_matrix(result$beta_vcov_adj)
  # Here we don't need the Jacobian list.
  expect_null(result$jac_list)
})

test_that("mmrm works for vcov: Jackknife and method: Satterthwaite", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Satterthwaite",
      vcov = "Empirical-Jackknife"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_matrix(result$beta_vcov_adj)
  # Here we don't need the Jacobian list.
  expect_null(result$jac_list)
})

test_that("mmrm fails for vcov: Asymptotic and method: Kenward-Roger", {
  expect_error(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Kenward-Roger",
      vcov = "Asymptotic"
    ),
    "Kenward-Roger degrees of freedom must work together with Kenward-Roger or Kenward-Roger-Linear covariance!"
  )
})

test_that("mmrm works for vcov: Kenward-Roger and method: Kenward-Roger", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Kenward-Roger",
      vcov = "Kenward-Roger"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_list(result$kr_comp, types = "matrix")
  expect_matrix(result$beta_vcov_adj)
})

test_that("mmrm works for vcov: Kenward-Roger-Linear and method: Kenward-Roger", {
  result <- expect_silent(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Kenward-Roger",
      vcov = "Kenward-Roger-Linear"
    )
  )
  expect_class(result, c("mmrm", "mmrm_fit", "mmrm_tmb"))
  expect_true(attr(result, "converged"))
  expect_class(result$call, "call")
  expect_true(result$reml)
  expect_list(result$kr_comp, types = "matrix")
  expect_matrix(result$beta_vcov_adj)
})

test_that("mmrm fails for vcov: Empirical and method: Kenward-Roger", {
  expect_error(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Kenward-Roger",
      vcov = "Empirical"
    ),
    "Kenward-Roger degrees of freedom must work together with Kenward-Roger or Kenward-Roger-Linear covariance!"
  )
})

test_that("mmrm fails for vcov: Jackknife and method: Kenward-Roger", {
  expect_error(
    mmrm(
      formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
      data = fev_data,
      method = "Kenward-Roger",
      vcov = "Empirical-Jackknife"
    ),
    "Kenward-Roger degrees of freedom must work together with Kenward-Roger or Kenward-Roger-Linear covariance!"
  )
})
