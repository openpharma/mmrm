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
  expected <- structure(
    matrix(
      c(465.89, 190.54, 190.54, 465.89),
      nrow = 2L, ncol = 2L,
      dimnames = list(c("AVISIT1", "AVISIT2"), c("AVISIT1", "AVISIT2"))
    ),
    stddev = c(AVISIT1 = 21.58, AVISIT2 = 21.58),
    correlation = matrix(
      c(1, 0.409, 0.409, 1),
      nrow = 2L, ncol = 2L,
      dimnames = list(c("AVISIT1", "AVISIT2"), c("AVISIT1", "AVISIT2"))
    ),
    blockCode = c(ar1 = 3),
    id = "2",
    n_parameters = 2L
  )
  expect_equal(result, expected, tolerance = 1e-3)
})
