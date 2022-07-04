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
