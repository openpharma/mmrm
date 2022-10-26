test_that("free_cores works as expected", {
  skip_on_cran()

  expect_silent(free_cores())
})
