# ante-dependence ----

test_that("prepare numbers for homogeneous ante_dependence tests", {
  theta <- c(log(2), 1, 2)
  n_visits <- 3
  sd <- exp(theta[1])
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
  result <- sd * t(chol(cor_mat))
  expected <- square_matrix(c(
    2.0, 0.0, 0.0,
    sqrt(2.0), sqrt(2.0), 0.0,
    1.264911, 1.264911, 0.8944272
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("prepare numbers for heterogeneous ante_dependence tests", {
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

# toeplitz ----

test_that("prepare numbers for homogeneous toeplitz tests", {
  theta <- c(log(2), 1, 2)
  n_visits <- 3
  sd <- exp(theta[1])
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
  result <- sd * t(chol(cor_mat))
  expected <- square_matrix(c(
    2.0, 0.0, 0.0,
    sqrt(2.0), sqrt(2.0), 0.0,
    1.788854, 0.2111456, 0.8691476
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("prepare numbers for heterogeneous toeplitz tests", {
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

# autoregressive ----

test_that("prepare numbers for autoregressive correlation function test", {
  x <- 1
  n_visits <- 3
  cor <- map_to_cor(x)
  expect_equal(cor, 1 / sqrt(2))
  expect_equal(cor^3, 0.3535534, tolerance = 1e-4)
})

test_that("prepare numbers for homogeneous autoregressive tests", {
  theta <- c(log(2), 3)
  n_visits <- 3
  cor <- map_to_cor(theta[2])
  cor_mat <- square_matrix(c(
    1, cor, cor^2,
    cor, 1, cor,
    cor^2, cor, 1
  ))
  sd <- exp(theta[1])
  result <- sd * t(chol(cor_mat))
  expected <- square_matrix(c(
    2, 0, 0,
    1.89736659610103, 0.632455532033676, 0,
    1.8, 0.6, 0.632455532033676
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("prepare numbers for heterogeneous autoregressive tests", {
  theta <- c(log(1), log(2), log(3), 2)
  n_visits <- 3
  sds <- exp(theta[1:n_visits])
  cor <- map_to_cor(theta[n_visits + 1])
  cor_mat <- square_matrix(c(
    1, cor, cor^2,
    cor, 1, cor,
    cor^2, cor, 1
  ))
  result <- diag(sds) %*% t(chol(cor_mat))
  expected <- square_matrix(c(
    1, 0, 0,
    1.78885438199983, 0.894427190999916, 0,
    2.4, 1.2, 1.34164078649987
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})

# compound symmetry ----

test_that("prepare numbers for compound symmetry correlation function test", {
  x <- 1.2
  cor <- map_to_cor(x)
  expect_equal(cor, 0.7682213, tolerance = 1e-4)
})

test_that("prepare numbers for homogeneous compound symmetry tests", {
  theta <- c(log(2), 3)
  n_visits <- 3
  cor <- map_to_cor(theta[2])
  cor_mat <- square_matrix(c(
    1, cor, cor,
    cor, 1, cor,
    cor, cor, 1
  ))
  sd <- exp(theta[1])
  result <- sd * t(chol(cor_mat))
  expected <- square_matrix(c(
    2, 0, 0,
    1.89736659610103, 0.632455532033676, 0,
    1.89736659610103, 0.307900211696917, 0.552446793489648
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("prepare numbers for heterogeneous compound symmetry tests", {
  theta <- c(log(1), log(2), log(3), 2)
  n_visits <- 3
  sds <- exp(theta[1:n_visits])
  cor <- map_to_cor(theta[n_visits + 1])
  cor_mat <- square_matrix(c(
    1, cor, cor,
    cor, 1, cor,
    cor, cor, 1
  ))
  result <- diag(sds) %*% t(chol(cor_mat))
  expected <- square_matrix(c(
    1, 0, 0,
    1.78885438199983, 0.894427190999916, 0,
    2.68328157299975, 0.633436854000505, 1.18269089452568
  ))
  expect_equal(result, expected, tolerance = 1e-5)
})
