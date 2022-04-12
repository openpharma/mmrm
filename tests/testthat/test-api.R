test_that("API greets the person", {
  host <- "127.0.0.1"
  port <- 9000

  # Start the API
  future::plan(future::multisession)
  future::future(
    mmrm::plumber_api(host = host, port = port)
  )
  Sys.sleep(3)

  # Make request
  res <- httr::GET(
    url = paste0(
      "http://",
      host,
      ":",
      port,
      "/echo"
    ),
    query = "name=tim"
  )

  # Get response
  result <- httr::content(res)[[1]]

  # Compare
  expected <- "Hello, Tim"
  expect_identical(result, expected)
})
