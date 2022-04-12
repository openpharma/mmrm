test_that("hello greets the entity", {
  result <- hello("foo")
  expected <- "Hello, Foo"
  expect_identical(result, expected)
})
