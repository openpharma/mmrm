test_that("with_error_call replaces top-level call in checkmate condition", {
  checks <- checkmate::makeAssertCollection()
  checks$push("check failed")

  # confirm undesirable checkmate behavior. if this ever fails, we can probably
  # remove `with_error_call`
  expect_true({
    f <- function() checkmate::reportAssertions(checks)
    e <- tryCatch(f(), error = identity)
    deparse(e$call[[1]])[[1]] != "f"
  })

  # error reports correct calling function
  expect_true({
    f <- function() with_error_call(checkmate::reportAssertions(checks))
    e <- tryCatch(f(), error = identity)
    deparse(e$call[[1]])[[1]] == "f"
  })

  # error reports from provided call
  expect_true({
    f <- function() {
      with_error_call(checkmate::reportAssertions(checks), sys.call(-1))
    }

    g <- function() f()
    e <- tryCatch(g(), error = identity)
    deparse(e$call[[1]])[[1]] == "g"
  })
})
