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
  expect_snapshot(result)
  expect_true(attr(result, "converged"))
  expect_false(identical("nlminb", attr(result, "optimizer")))
  expect_true(logLik(result) > logLik(fit))
})
