test_that("h_match_coefs matches permuted two-way and three-way interactions", {
  coef_names <- c("(Intercept)", "A:B", "A:B:C", "Z:Y:X", "Main")
  model_colnames <- c("(Intercept)", "B:A", "C:B:A", "X:Z:Y", "Main")

  expect_equal(
    h_match_coefs(coef_names, model_colnames),
    c(1L, 2L, 3L, 4L, 5L)
  )
})

test_that("h_match_coefs keeps direct matching behavior and returns NA for missing terms", {
  expect_equal(
    h_match_coefs("A:B", c("A:B", "B:A")),
    1L
  )

  expect_equal(
    h_match_coefs(c("A", "B:C", "M:N"), c("A", "C:B", "Q:R")),
    c(1L, 2L, NA_integer_)
  )
})
