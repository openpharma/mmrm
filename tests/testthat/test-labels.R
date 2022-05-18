library(testthat)

test_that("h_is_specified works as expected", {
  vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
  result <- h_is_specified("arm", vars)
  expect_true(result)
})

test_that("h_is_specified_and_in_data works as expected", {
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    ARM = factor(c(1, 2, 3, 1, 2, 3)),
    AVAL = c(2, 4, 6, 8, 10, 12),
    AVISIT = c("W1", "W2", "W3", "W1", "W2", "W3"),
    SEX = c("Female", "Female", "Female", "Male", "Male", "Male"),
    RACE = c("Asian", "Asian", "Asian", "White", "Asian", "White")
  )
  vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
  result <- h_is_specified_and_in_data("arm", vars, data)
  expect_true(result)
})

test_that("h_check_and_get_label works as expected", {
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    ARM = factor(c(1, 2, 3, 1, 2, 3)),
    AVAL = c(2, 4, 6, 8, 10, 12),
    AVISIT = c("W1", "W2", "W3", "W1", "W2", "W3"),
    SEX = c("Female", "Female", "Female", "Male", "Male", "Male"),
    RACE = c("Asian", "Asian", "Asian", "White", "Asian", "White")
  )
  vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
  result <- h_check_and_get_label("arm", vars, data)
  expected <- c("ARM")
  names(expected) <- "ARM"
  expect_identical(result, expected)
})

test_that("h_labels works as expected", {
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    ARM = factor(c(1, 2, 3, 1, 2, 3)),
    AVAL = c(2, 4, 6, 8, 10, 12),
    AVISIT = c("W1", "W2", "W3", "W1", "W2", "W3"),
    SEX = c("Female", "Female", "Female", "Male", "Male", "Male"),
    RACE = c("Asian", "Asian", "Asian", "White", "Asian", "White")
  )
  vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
  result <- h_labels(vars, data)
  expected <- list(
    response = "AVAL",
    id = "MYID",
    visit = "AVISIT",
    parts = c("SEX", "RACE")
  )
  names(expected) <- c("response", "id", "visit", "parts")
  names(expected$response) <- "AVAL"
  names(expected$id) <- "MYID"
  names(expected$visit) <- "AVISIT"
  names(expected$parts) <- c("SEX", "RACE")
  expect_identical(result, expected)
})
