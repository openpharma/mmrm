library(testthat)

test_that("h_build_formula gives error message if incorrect covariance structure is given", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  expect_error(h_build_formula(vars, "BLAHHH"))
})


test_that("h_build_formula gives no message if input is correct", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  expect_silent(h_build_formula(vars, "compound-symmetry"))
})
