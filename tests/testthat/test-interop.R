test_that("check_package_version warns when version is not compatible", {
  skip_if_not_installed("mockery")
  mockery::stub(check_package_version, "utils::packageVersion", numeric_version("1.2.3"))

  # When no version requirements are specified, checking package always succeeds
  expect_true(check_package_version("fakePackage"))

  # Warns when a minimum version isn't met
  expect_warning(check_package_version("fakePackage", c("10", NA)), "fakePackage")

  # Warns when a maximum version isn't met
  expect_warning(check_package_version("fakePackage", c(NA, "1")), "fakePackage")
})

test_that("register_on_load sets an onLoad hook when package is not yet loaded", {
  expect_silent(register_on_load("fakePackage", callback = function() NULL))
  expect_true(!is.null(getHook(packageEvent("fakePackage", "onLoad"))))
})

test_that("check_package_version rejects numeric version input", {
  expect_error(
    check_package_version("fakePackage", c(1, 2)),
    "Must be of type 'character'"
  )
})
