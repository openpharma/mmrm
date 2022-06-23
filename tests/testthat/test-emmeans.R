test_that("recover_data method works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.7")

  fit <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- emmeans::recover_data(fit)
  expect_data_frame(result, nrows = nrow(fit$tmb_data$x_matrix))
  expect_named(
    attributes(result),
    c("names", "row.names", "class", "call", "terms", "predictors", "responses")
  )
})
