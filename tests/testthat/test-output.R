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
