# h_record_all_outputs ----

test_that("h_record_all_outputs correctly removes specified messages", {
  result <- h_record_all_output({
    x <- 1
    y <- 2
    warning("something went wrong")
    message("O nearly done")
    message("Almost done")
    x + y
  }, remove = list(messages = c("Almost done", "bla")))
  expected <- list(result = 3, warnings = "something went wrong", errors = NULL, messages = "O nearly done")
  expect_identical(result, expected)
})

test_that("h_record_all_outputs works as expected with no removal list given for messages", {
  result <- h_record_all_output({
    x <- 1
    y <- 2
    warning("something went wrong")
    message("O nearly done")
    message("oh noo")
    x + y
  })
  expected <- list(result = 3, warnings = "something went wrong", errors = NULL,
  messages = c("O nearly done", "oh noo"))
  expect_identical(result, expected)
})


# h_tr ----

test_that("trace of a matrix works as expected", {
  mx <- matrix(0, nrow = 3, ncol = 4)
  expect_error(h_tr(mx), "x must be square matrix")
  v <- c(1, 3, 2)
  expect_equal(h_tr(diag(v)), 6)
})

# free_cors ----

test_that("free_cores throws deprecation warning", {
  skip_on_cran()
  expect_warning(
    free_cores(),
    regexp = "`free_cores()` was deprecated in mmrm 0.1.6.",
    fixed = TRUE
  )
})
