test_that("fit_single_optimizer works as expected with optimizer inputted but no starting values", {
  skip("refactoring")

  dat <- mmrm::fev_data
  form <- FEV1 ~ ar1(0 + AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat,
    optimizer = "BFGS"
  )
  expect_identical(class(result), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result, "optimizer"), "BFGS")
  expect_identical(attr(result, "errors"), NULL)
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer works as expected with starting values and optimizer inputted", {
  skip("refactoring")

  dat <- mmrm::fev_data
  form <- FEV1 ~ ar1(0 + AVISIT | USUBJID)
  result <- fit_single_optimizer(
    formula = form,
    data = dat,
    optimizer = "BFGS",
    start = list(theta = c(3, 0.5))
  )
  expect_identical(class(result), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result, "optimizer"), "BFGS")
  expect_identical(attr(result, "errors"), NULL)
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer works as expected with no starting values or optimizer inputted", {
  skip("refactoring")

  dat <- mmrm::fev_data
  form <- FEV1 ~ ar1(0 + AVISIT | USUBJID)
  result <- fit_single_optimizer(
      formula = form,
      data = dat
    )
  expect_identical(class(result), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result, "optimizer"), "L-BFGS-B")
  expect_identical(attr(result, "errors"), NULL)
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})

test_that("fit_single_optimizer works as expected with starting values but no optimizer inputted", {
  skip("refactoring")

  dat <- mmrm::fev_data
  form <- FEV1 ~ ar1(0 + AVISIT | USUBJID)
  result <- fit_single_optimizer(
      formula = form,
      data = dat,
      start = list(theta = c(3, 0.5))
    )
  expect_identical(class(result), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result, "optimizer"), "L-BFGS-B")
  expect_identical(attr(result, "errors"), NULL)
  expect_identical(attr(result, "messages"), NULL)
  expect_identical(attr(result, "warnings"), NULL)
  expect_true(attr(result, "converged"))
})
