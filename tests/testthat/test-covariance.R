test_that("h_cov_estimate works as expected", {
  data <- data.frame(
    MYID = as.factor(c(1, 1, 1, 2, 2, 3, 3)),
    AVISIT = as.factor(c(1, 2, 2, 1, 2, 1, 2)),
    RESP = c(20, 30, 40, 50, 60, 70, 80)
    )
  model <- glmmTMB::glmmTMB(
    RESP ~ ar1(0 + AVISIT | MYID),
    data = data,
    dispformula = ~0,
    REML = TRUE
  )
  class(model) <- c("mmrm_fit", "glmmTMB")
  result <- h_cov_estimate(model)
  expected <- structure(glmmTMB::VarCorr(model)$cond[[1L]], id = "2", n_parameters = as.integer(2))
  expect_identical(result, expected)
})
