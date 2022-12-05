# trace of matrix

test_that("trace of a matrix works as expected", {
  mx <- matrix(0, nrow = 3, ncol = 4)
  expect_error(h_tr(mx), "x must be square matrix")
  v <- c(1, 3, 2)
  expect_equal(h_tr(diag(v)), 6)
})
