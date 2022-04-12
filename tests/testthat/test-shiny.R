test_that("The Shiny App returns a proper greeting", {
  library(shinytest)
  app <- ShinyDriver$new(
    "shiny-app/",
    loadTimeout = 1e5,
    debug = "all",
    phantomTimeout = 1e5,
    seed = 123
  )
  app$getDebugLog()

  # Set input
  app$setInputs(name = "john")
  app$setInputs(greet = "click")
  output <- app$getValue(name = "greeting")

  # test
  expect_equal(output, "Hello, John")

  # wait for the process to close gracefully
  # this allows covr to write out the coverage results
  p <- app$.__enclos_env__$private$shinyProcess
  p$interrupt()
  p$wait()
})
