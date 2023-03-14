# get_corr_mat_chol ----

test_that("prepare numbers for get_corr_mat_chol tests", {
  cor_mat <- matrix(data = 0.5, nrow = 3, ncol = 3)
  diag(cor_mat) <- 1
  result <- t(chol(cor_mat))
  expected <- square_matrix(c(
    1, 0, 0,
    0.5, 0.866025403784439, 0,
    0.5, 0.288675134594813, 0.816496580927726
  ))
  expect_equal(result, expected)
})

test_that("prepare numbers for pseudoInverseSqrt", {
  input <- matrix(c(
    5.483417, 2.861011, 3.478399,
    2.861011, 3.169936, -1.075550,
    3.478399, -1.075550, 10.525825
  ), 3, 3)
  ed <- eigen(solve(input))
  result <- ed$vectors %*% diag(sqrt(ed$values)) %*% t(ed$vectors)
  expected <- matrix(c(
    0.8235633, -0.5514385, -0.2586037,
    -0.5514385, 1.0568775, 0.2548210,
    -0.2586037, 0.2548210, 0.4095994
  ), 3, 3)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("prepare numbers for pseudoInverseSqrt rank deficient", {
  skip_if_not_installed("MASS")
  input <- matrix(c(
    5.483417, 2.861011, 0,
    2.861011, 3.169936, 0,
    0, 0, 0
  ), 3, 3)
  ed <- eigen(MASS::ginv(input))
  result <- ed$vectors %*% diag(sqrt(ed$values)) %*% t(ed$vectors)
  expected <- matrix(c(
    0.5331152, -0.2459070, 0.0,
    -0.2459070, 0.7319613, 0.0,
    0.0000000, 0.0000000, 0.0
  ), 3, 3)
  expect_equal(result, expected, tolerance = 1e-4)
})
