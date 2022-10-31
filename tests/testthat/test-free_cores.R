test_that("free_cores throws deprecation warning", {
  skip_on_cran()
  expect_warning(
    free_cores(),
    regexp = "`free_cores\\(\\)` was deprecated in mmrm 0\\.1\\.6."
  )
})
