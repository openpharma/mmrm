# h_get_covariate_parts ----

test_that("h_get_covariate_parts works as expected", {
  expect_null(h_get_covariate_parts(NULL))
  expect_identical(h_get_covariate_parts("bla"), "bla")
  expect_identical(h_get_covariate_parts("bla:FOO"), c("bla", "FOO"))
  expect_identical(h_get_covariate_parts(c("bla:FOO", "X")), c("bla", "FOO", "X"))
  expect_identical(h_get_covariate_parts(c("bla*FOO", "X")), c("bla", "FOO", "X"))
})
