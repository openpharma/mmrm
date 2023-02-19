test_that("mean_time_fun computes the mean fit time", {
  # generate a fit_results tibble
  fit_results <- tibble(
    .method_name = c(rep("a", 3), rep("b", 3), rep("a", 3)),
    .dgp_name = c(rep("dgp1", 3), rep("dgp1", 3), rep("dgp2", 3)),
    n_obs = c(rep(100, 3), rep(200, 3), rep(100, 3)),
    fit_time = c(5, 10, 15, 1, 2, 3, 5, 10, 15)
  )

  # compute the mean time to computer per method, per sample size, per DGP
  mean_times_tbl <- mean_time_fun(fit_results)

  expect_equal(mean_times_tbl$mean_time, c(10, 2, 10))

})
