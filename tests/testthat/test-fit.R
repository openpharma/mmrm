test_that("fit_single_optimizer works as expected with no warnings, errors, or messages", {
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

  result2 <- fit_single_optimizer(
    formula = form,
    data = dat,
    optimizer = "BFGS",
    start = list(theta = c(3, 0.5))
  )
  expect_identical(class(result2), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result2, "optimizer"), "BFGS")
  expect_identical(attr(result2, "errors"), NULL)
  expect_identical(attr(result2, "messages"), NULL)
  expect_identical(attr(result2, "warnings"), NULL)
  expect_true(attr(result2, "converged"))

  result3 <-
    fit_single_optimizer(
      formula = form,
      data = dat
    )
  expect_identical(class(result3), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result3, "optimizer"), "L-BFGS-B")
  expect_identical(attr(result3, "errors"), NULL)
  expect_identical(attr(result3, "messages"), NULL)
  expect_identical(attr(result3, "warnings"), NULL)
  expect_true(attr(result3, "converged"))

  result4 <-
    fit_single_optimizer(
      formula = form,
      data = dat,
      start = list(theta = c(3, 0.5))
    )
  expect_identical(class(result4), c("mmrm_fit", "glmmTMB"))
  expect_identical(attr(result4, "optimizer"), "L-BFGS-B")
  expect_identical(attr(result4, "errors"), NULL)
  expect_identical(attr(result4, "messages"), NULL)
  expect_identical(attr(result4, "warnings"), NULL)
  expect_true(attr(result4, "converged"))
})

