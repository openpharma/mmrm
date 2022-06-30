# get_ante_dependence ----

test_that("prepare numbers for get_ante_dependence test", {
  theta <- c(log(1), log(2), log(3), 1, 2)
  n_visits <- 3
  sds <- exp(theta[1:n_visits])
  x <- tail(theta, n_visits - 1)
  cors <- x / sqrt(1 + x^2)
  cov_mat <- square_matrix(c(
    sds[1]^2, sds[1] * sds[2] * cors[1], sds[1] * sds[3] * prod(cors[1:2]),
    sds[1] * sds[2] * cors[1], sds[2]^2, sds[2] * sds[3] * cors[2],
    sds[1] * sds[3] * prod(cors[1:2]), sds[2] * sds[3] * cors[2], sds[3]^2
  ))
  result <- t(chol(cov_mat))
  expected <- square_matrix(c(
    1, 0, 0,
    sqrt(2), sqrt(2), 0,
    1.897367, 1.897367, 1.341641
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})
