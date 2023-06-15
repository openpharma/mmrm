# cov_struct ----

test_that("cov_struct can be produced from string variable names", {
  expect_class(
    cov_struct("us", "visit", "subject", "group"),
    "cov_struct"
  )

  expect_class(
    cov_struct("Toeplitz", "visit", "subject"),
    "cov_struct"
  )

  expect_true(
    cov_struct("toeph", "visit", "subject")$heterogeneous
  )

  expect_false(
    cov_struct("toep", "visit", "subject")$heterogeneous
  )
})

test_that("cov_struct raises errors for common mis-specifications", {
  expect_error(
    cov_struct("us", c("vA", "vB"), "subject"),
    "Non-spatial covariance .* single longitudinal"
  )

  expect_error(
    cov_struct("us", "visit", "subject", c("gA", "gB")),
    "Covariance .* must be of the form"
  )

  expect_error(
    cov_struct("csh", "visit", c("sA", "sB"), "group"),
    "'subject': Must have length 1"
  )

  expect_error(
    cov_struct("invalid", "visit", "subject", "group"),
    "should be one of"
  )
})

# as.cov_struct.cov_struct ----

test_that("as.cov_struct returns self if already a cov_struct", {
  expect_identical(
    cs <- cov_struct("us", "visit", "subject"),
    cs
  )
})

# as.cov_struct.formula ----

test_that("as.cov_struct can derive a covariance structure from a formula", {
  expect_identical(
    as.cov_struct(y ~ x + us(visit | subject)),
    cov_struct("us", "visit", "subject")
  )

  expect_identical(
    as.cov_struct(y ~ x + csh(visit | subject)),
    cov_struct("cs", "visit", "subject", heterogeneous = TRUE)
  )

  expect_identical(
    as.cov_struct(~ x + toeph(visit | group / subject) + z),
    cov_struct("toeph", "visit", "subject", "group")
  )
  expect_identical(
    as.cov_struct(~ x:z + toeph(visit | group / subject) + m:n),
    cov_struct("toeph", "visit", "subject", "group")
  )
})

# tmb_cov_type ----

test_that("tmb_cov_type derives abbreviated string from covariance structure", {
  expect_identical(
    tmb_cov_type(cov_struct("cs", "visit", "subject", heterogeneous = TRUE)),
    "csh"
  )

  expect_identical(
    tmb_cov_type(cov_struct("toeph", "visit", "subject")),
    "toeph"
  )
})

# cov_types ----

test_that("cov_types returns list of covariance structure keywords", {
  expect_true(all(c("Toeplitz", "ante-dependence", "ar1", "sp_exp") %in% cov_types()))
  expect_false("Toeplitz" %in% cov_types("abbr"))
  expect_true("csh" %in% cov_types("habbr"))
  expect_true("sp_exp" %in% cov_types(filter = "spatial"))
})

# cov_types_abbr ----

test_that("cov_type_abbr converts full names to abbreviations", {
  expect_identical(cov_type_abbr("Toeplitz"), "toep")
  expect_identical(cov_type_abbr("auto-regressive order one"), "ar1")
  expect_identical(cov_type_abbr("csh"), "cs")
  expect_identical(cov_type_abbr("ad"), "ad")
})

# cov_types_name ----

test_that("cov_type_name converts full names to full name", {
  expect_identical(cov_type_name("toep"), "Toeplitz")
  expect_identical(cov_type_name("ar1"), "auto-regressive order one")
  expect_identical(cov_type_name("ante-dependence"), "ante-dependence")
})

# print.cov_struct, format.cov_struct ----

test_that("print.cov_struct pretty-formats covariance structure object", {
  expect_output(
    print(cov_struct("csh", "VISIT", "SUBJECT", "GROUP")),
    "<covariance structure>.*heterogeneous compound symmetry:.*"
  )

  expect_output(
    print(cov_struct("csh", "VISIT", "SUBJECT", "GROUP")),
    "VISIT | GROUP / SUBJECT"
  )

  expect_output(
    print(cov_struct("sp_exp", c("VIS IT", "VISIT"), "SUBJECT", "GROUP")),
    "`VIS IT`, VISIT | GROUP / SUBJECT"
  )
})

# h_reconcile_cov_struct ----

test_that("h_reconcile_cov_struct works if covariance is a formula", {
  cov <- expect_silent(h_reconcile_cov_struct(a ~ b, ~ ar1(a | b)))
  expected <- cov_struct("ar1", "a", "b")
  expect_identical(cov, expected)
})

test_that("h_reconcile_cov_struct works if covariance is a cov_struct", {
  expected <- cov_struct("ar1", "a", "b")
  cov <- expect_silent(h_reconcile_cov_struct(a ~ b, expected))
  expect_identical(cov, expected)
})

test_that("h_reconcile_cov_struct errors if multiple covarinace spotted", {
  expect_error(h_reconcile_cov_struct(a ~ b + ar1(a | b), ~ ar1(a | b)))
})
