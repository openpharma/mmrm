# get_ante_dependence ----

test_that("prepare numbers for ante_dependence tests", {
  theta <- c(log(1), log(2), log(3), 1, 2)
  n_visits <- 3
  sds <- exp(theta[1:n_visits])
  x <- tail(theta, n_visits - 1)
  cors <- x / sqrt(1 + x^2)
  cor_mat <- square_matrix(c(
    1, cors[1], prod(cors[1:2]),
    cors[1], 1, cors[2],
    prod(cors[1:2]), cors[2], 1
  ))
  low_tri <- lower.tri(cor_mat)
  cor_fun_vals <- cbind(
    row(cor_mat)[low_tri] - 1,
    col(cor_mat)[low_tri] - 1,
    cor_mat[low_tri]
  )
  result <- diag(sds) %*% t(chol(cor_mat))
  expected <- square_matrix(c(
    1, 0, 0,
    sqrt(2), sqrt(2), 0,
    1.897367, 1.897367, 1.341641
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("prepare numbers for toeplitz tests", {
  theta <- c(log(1), log(2), log(3), 1, 2)
  n_visits <- 3
  sds <- exp(theta[1:n_visits])
  x <- tail(theta, n_visits - 1)
  cors <- x / sqrt(1 + x^2)
  cor_mat <- square_matrix(c(
    1, cors[1], cors[2],
    cors[1], 1, cors[1],
    cors[2], cors[1], 1
  ))
  low_tri <- lower.tri(cor_mat)
  cor_fun_vals <- cbind(
    row(cor_mat)[low_tri] - 1,
    col(cor_mat)[low_tri] - 1,
    cor_mat[low_tri]
  )
  result <- diag(sds) %*% t(chol(cor_mat))
  expected <- square_matrix(c(
    1, 0, 0,
    sqrt(2), sqrt(2), 0,
    2.683282, 0.3167184, 1.303721
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})
