# is_r_devel ----

test_that("is_r_devel works as expected", {
  expect_flag(is_r_devel())
})

# is_linux ----

test_that("is_linux works as expected", {
  expect_flag(is_linux())
})

# is_using_clang ----

test_that("is_using_clang works as expected", {
  expect_flag(is_using_clang())
})

# parse_clang_major ----

test_that("parse_clang_major works as expected", {
  result <- parse_clang_major("Debian clang version 17.0.6 (3)")
  expected <- 17L
  expect_identical(result, expected)
})

# is_non_standard_clang ----

test_that("is_non_standard_clang works as expected for Fedora", {
  os_string <- "Fedora Linux 36 (Workstation Edition)"
  expect_false(is_non_standard_clang(os_string, clang_major_version = 14L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 13L))
  expect_true(is_non_standard_clang(os_string, clang_major_version = 15L))
})

test_that("is_non_standard_clang returns FALSE for non-listed Fedora versions", {
  os_string <- "Fedora Linux 12"
  expect_false(is_non_standard_clang(os_string, clang_major_version = 14L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 13L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 15L))
})

test_that("is_non_standard_clang works as expected for Debian", {
  os_string <- "Debian GNU/Linux trixie/sid"
  expect_false(is_non_standard_clang(os_string, clang_major_version = 16L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 15L))
  expect_true(is_non_standard_clang(os_string, clang_major_version = 17L))
})

test_that("is_non_standard_clang returns FALSE for non-listed Debian versions", {
  os_string <- "Debian GNU/Linux bla"
  expect_false(is_non_standard_clang(os_string, clang_major_version = 14L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 13L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 15L))
})

test_that("is_non_standard_clang returns FALSE for other Linux distributions", {
  os_string <- "Solaris"
  expect_false(is_non_standard_clang(os_string, clang_major_version = 14L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 13L))
  expect_false(is_non_standard_clang(os_string, clang_major_version = 15L))
})

# is_r_devel_linux_clang ----

test_that("is_r_devel_linux_clang works as expected", {
  expect_flag(is_r_devel_linux_clang())
})
