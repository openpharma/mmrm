# h_extract_covariance_terms ----

test_that("h_extract_covariance_terms returns a list of possible covariance terms", {
  expect_identical(
    h_extract_covariance_terms(a ~ b + us(a | g / s) + other(b, c)),
    list(us = quote(us(a | g / s)))
  )

  expect_identical(
    h_extract_covariance_terms(a ~ b + us(a | g / s) + csh(a2 | g2 / s2)),
    list(
      us = quote(us(a | g / s)),
      csh = quote(csh(a2 | g2 / s2))
    )
  )

  expect_identical(
    h_extract_covariance_terms(~ b + us(a | g / s) + csh(a2 | g2 / s2)),
    list(
      us = quote(us(a | g / s)),
      csh = quote(csh(a2 | g2 / s2))
    )
  )

  expect_identical(
    h_extract_covariance_terms(a ~ a + us(a | g / s)),
    list(us = quote(us(a | g / s)))
  )

  expect_identical(
    h_extract_covariance_terms(a ~ c:d + us(a | g / s)),
    list(us = quote(us(a | g / s)))
  )
  expect_identical(
    h_extract_covariance_terms(a ~ c:d + us(a | g / s) + csh(a2 | g2 / s2)),
    list(
      us = quote(us(a | g / s)),
      csh = quote(csh(a2 | g2 / s2))
    )
  )
})

# h_drop_covariance_terms ----

test_that("h_drop_covariance_terms identifies covariance terms specifically", {
  expect_identical(
    h_drop_covariance_terms(a ~ b + us(a | g / s)),
    a ~ b
  )

  expect_identical(
    h_drop_covariance_terms(~ b + us(a | g / s)),
    ~b
  )

  expect_identical(
    h_drop_covariance_terms(a ~ b + csh(a | g / s) + c),
    a ~ b + c
  )

  expect_identical(
    h_drop_covariance_terms(a ~ b + other(a | g / s) + c),
    a ~ b + other(a | g / s) + c
  )
})

# h_add_covariance_terms ----

test_that("h_add_covariance_terms adds covariance variables as terms", {
  cs <- cov_struct("us", "AVISITN", "USUBJID")
  f <- a ~ b + c

  expect_identical(
    h_add_covariance_terms(f, cs),
    a ~ b + c + USUBJID + AVISITN
  )

  cs <- cov_struct("sp_exp", c("COORDA", "COORDB"), "USUBJID", "GRP")
  f <- a ~ b + c

  expect_identical(
    h_add_covariance_terms(f, cs),
    a ~ b + c + USUBJID + COORDA + COORDB + GRP
  )
})

# h_add_terms ----

test_that("h_drop_terms works", {
  formula <- x ~ y + z
  expect_identical(
    h_add_terms(formula, "a"),
    x ~ y + z + a
  )
  expect_identical(
    h_add_terms(formula, c("y", "z")),
    x ~ y + z
  )
  expect_identical(
    h_add_terms(formula, "y*z"),
    x ~ y + z + y:z
  )
})

test_that("h_add_terms works on adds is length 0", {
  formula <- x ~ y + z
  expect_identical(
    h_add_terms(formula, NULL),
    formula
  )
  expect_identical(
    h_add_terms(formula, character(0)),
    formula
  )
})
