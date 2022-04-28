test_that("dummy works", {
  library(mmrm)
  input <- 5
  result <- dummy(input)
  expected <- input
  expect_identical(result, expected)
})
