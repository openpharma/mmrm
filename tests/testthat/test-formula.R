library(testthat)

test_that("h_build_formula gives error message if incorrect covariance structure is given", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  expect_error(h_build_formula(vars, "BLAHHH"))
})


test_that("h_build_formula builds the correct formula", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  result <- as.character(h_build_formula(vars, "compound-symmetry"))
  expected <- as.character(AVAL ~ RACE + SEX + ARMCD * AVISIT + cs(0 + AVISIT | USUBJID))
  expect_identical(result, expected)
})

test_that("h_build_formula builds the correct formula with no arm variable given", {
  vars <- list(response = "AVAL", id = "USUBJID", visit = "AVISIT", covariates = c("RACE", "SEX"))
  result <- as.character(h_build_formula(vars, "compound-symmetry"))
  expected <- as.character(AVAL ~ RACE + SEX +  AVISIT + cs(0 + AVISIT | USUBJID))
  expect_identical(result, expected)
})

test_that("h_build_formula builds the correct formula with no covariates given", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT")
  result <- as.character(h_build_formula(vars, "compound-symmetry"))
  expected <- as.character(AVAL ~ ARMCD * AVISIT + cs(0 + AVISIT | USUBJID))
  expect_identical(result, expected)
})
