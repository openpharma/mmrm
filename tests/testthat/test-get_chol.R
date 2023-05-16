# get_chol ----

test_that("get_chol works correctly", {
  fit <- get_mmrm()
  expect_identical(
    fit$tmb_object$report()$cholesky_all,
    get_chol(fit, fev_data, NULL, fit$theta_est)
  )
  new_theta <- seq_len(length(fit$theta_est)) / length(fit$theta_est)
  expect_identical(
    fit$tmb_object$report(new_theta)$cholesky_all,
    get_chol(fit, fev_data, NULL, new_theta)
  )
})
