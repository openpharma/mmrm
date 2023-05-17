# h_get_chol ----

test_that("h_get_chol works correctly", {
  fit <- get_mmrm()
  expect_identical(
    fit$tmb_object$report()$cholesky_all,
    h_get_chol(fit)
  )
  new_theta <- seq_len(length(fit$theta_est)) / length(fit$theta_est)
  expect_identical(
    fit$tmb_object$report(new_theta)$cholesky_all,
    h_get_chol(fit, theta = new_theta, data = fev_data, weights = NULL)
  )
})

test_that("h_get_chol works correctly to provide cholesky on all visits", {
  fit <- get_mmrm()
  new_theta <- seq_len(length(fit$theta_est)) / length(fit$theta_est)
  chols <- expect_silent(
    h_get_chol(fit, theta = new_theta, data = fev_data, weights = NULL, complete_case = FALSE)
  )
  lapply(
    chols,
    expect_matrix,
    ncols = 4,
    nrows = 4
  )
})

test_that("h_get_chol errors on non correct theta", {
  fit <- get_mmrm()
  expect_error(
    h_get_chol(fit, theta = 1),
    "Must have length 10"
  )
})

test_that("h_get_chol errors on non correct data", {
  fit <- get_mmrm()
  expect_error(
    h_get_chol(fit, data = iris),
    "Names must include the elements"
  )
})

test_that("h_get_chol works with visit var as character subset of original fit", {
  fit <- get_mmrm()
  data <- data.frame(
    USUBJID = rep(as.character(1:4), c(3, 2, 1, 3)),
    AVISIT = sprintf("VIS%s", c(1, 2, 3, 2, 3, 3, 1, 2, 3))
  )
  expect_silent(
    h_get_chol(fit, data = data)
  )
})

test_that("h_get_chol errors with visit var as factor with different levels", {
  fit <- get_mmrm()
  data <- data.frame(
    USUBJID = rep(as.character(1:4), c(3, 2, 1, 3)),
    AVISIT = factor(sprintf("VIS%s", c(1, 2, 5, 2, 3, 3, 1, 2, 3)))
  )
  expect_error(
    h_get_chol(fit, data = data),
    "but has additional elements {'VIS5'}",
    fixed = TRUE
  )
})

test_that("h_get_chol works with visit var, subject var and group var", {
  fit <- get_mmrm_group()
  data <- data.frame(
    USUBJID = rep(as.character(1:4), c(3, 2, 1, 3)),
    AVISIT = sprintf("VIS%s", c(1, 2, 3, 2, 3, 3, 1, 2, 3)),
    ARMCD = factor(rep(c("TRT", "PBO", "PBO", "TRT"), c(3, 2, 1, 3)), levels = c("PBO", "TRT"))
  )
  expect_silent(
    h_get_chol(fit, data = data)
  )
  data <- data.frame(
    USUBJID = rep(as.character(1:4), c(3, 2, 1, 3)),
    AVISIT = sprintf("VIS%s", c(1, 2, 3, 2, 3, 3, 1, 2, 3)),
    ARMCD = "TRT"
  )
  expect_silent(
    h_get_chol(fit, data = data)
  )
})

test_that("h_get_chol works with grouped fit and theta", {
  fit <- get_mmrm_group()
  data <- data.frame(
    USUBJID = rep(as.character(1:4), c(3, 2, 1, 3)),
    AVISIT = sprintf("VIS%s", c(1, 2, 3, 2, 3, 3, 1, 2, 3)),
    ARMCD = "TRT"
  )
  theta_new <- fit$theta_est
  theta_new[1:10] <- 0
  expect_identical(
    h_get_chol(fit, data = data, theta = theta_new),
    h_get_chol(fit, data = data, theta = fit$theta_est)
  )
})
