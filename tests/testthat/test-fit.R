# fit_single_optimizer ----

test_that("fit_single_optimizer works as expected with defaults", {
  dat <- fev_data
  form <- FEV1 ~ ARMCD * RACE + us(AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "L-BFGS-B")
  expect_identical(attr(result, "errors"), NULL)
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
    optimizer = "BFGS"
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "BFGS")
  expect_identical(attr(result, "errors"), NULL)
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
    optimizer = "BFGS",
    start = 1:10
  )
  expect_identical(class(result), c("mmrm_fit", "mmrm_tmb"))
  expect_identical(attr(result, "optimizer"), "BFGS")
  expect_identical(attr(result, "errors"), NULL)
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})
