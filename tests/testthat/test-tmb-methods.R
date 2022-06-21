get_mmrm_tmb <- function() {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  data <- fev_data
  mmrm_tmb(formula, data)
}


# logLik ----

test_that("logLik works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(logLik(object))
  expected <- -1821.19736
  expect_equal(result, expected)
})
