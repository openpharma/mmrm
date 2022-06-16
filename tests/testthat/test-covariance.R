test_that("h_cov_estimate works as expected", {
  mod_fit <- fit_single_optimizer(
    formula = FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- h_cov_estimate(mod_fit)
  expected <- structure(
    matrix(
      c(108.41, 45.4, -2.71, -47.03, 45.4, 40.16, 0.78, -13.09, -2.71,
        0.78, 24.61, 18.74, -47.03, -13.09, 18.74, 152.38),
      nrow = 4L, ncol = 4L,
      dimnames = list(
        c("VIS1", "VIS2", "VIS3", "VIS4"),
        c("VIS1", "VIS2", "VIS3", "VIS4")
      )
    ),
    n_parameters = 10L
  )
  expect_equal(result, expected, tolerance = 1e-3)
})
