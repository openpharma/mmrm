# is_r_devel ----

test_that("is_r_devel works as expected", {
  expect_flag(is_r_devel())
})

# is_linux ----

test_that("is_linux works as expected", {
  expect_flag(is_linux())
})

# get_compiler ----

test_that("get_compiler works as expected", {
  expect_string(get_compiler())
})

# is_using_clang ----

test_that("is_using_clang works as expected", {
  expect_flag(is_using_clang())
})

test_that("is_using_clang gives the same information as R_compiled_by in recent R versions", {
  skip_if(getRversion() < "4.3")
  result <- is_using_clang()
  expected <- grepl("clang", R_compiled_by()["C"]) # Only available from R 4.3 onward.
  expect_identical(result, expected)
})

# is_r_devel_linux_clang ----

test_that("is_r_devel_linux_clang works as expected", {
  expect_flag(is_r_devel_linux_clang())
})
