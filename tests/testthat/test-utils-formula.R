# extract_covariance_terms ----

test_that("extract_covariance_terms returns a list of possible covariance terms", {
  expect_identical(
    extract_covariance_terms(a ~ b + us(a | g / s) + other(b, c)),
    list(us = quote(us(a | g / s)))
  )

  expect_identical(
    extract_covariance_terms(a ~ b + us(a | g / s) + csh(a2 | g2 / s2)),
    list(
      us = quote(us(a | g / s)),
      csh = quote(csh(a2 | g2 / s2))
    )
  )

  expect_identical(
    extract_covariance_terms(~ b + us(a | g / s) + csh(a2 | g2 / s2)),
    list(
      us = quote(us(a | g / s)),
      csh = quote(csh(a2 | g2 / s2))
    )
  )

  expect_identical(
    extract_covariance_terms(a ~ a + us(a | g / s)),
    list(us = quote(us(a | g / s)))
  )
})

# drop_covariance_terms ----

test_that("drop_covariance_terms identifies covariance terms specifically", {
  expect_identical(
    drop_covariance_terms(a ~ b + us(a | g / s)),
    a ~ b
  )

  expect_identical(
    drop_covariance_terms(~ b + us(a | g / s)),
    ~ b
  )

  expect_identical(
    drop_covariance_terms(a ~ b + csh(a | g / s) + c),
    a ~ b + c
  )

  expect_identical(
    drop_covariance_terms(a ~ b + other(a | g / s) + c),
    a ~ b + other(a | g / s) + c
  )
})

# add_covariance_variable_terms ----

test_that("add_covariance_variable_terms adds covariance variables as terms", {
  cs <- cov_struct("us", "AVISITN", "USUBJID")
  f <- a ~ b + c

  expect_identical(
    add_covariance_variable_terms(f, cs),
    a ~ b + c + USUBJID + AVISITN
  )

  cs <- cov_struct("sp_exp", c("COORDA", "COORDB"), "USUBJID", "GRP")
  f <- a ~ b + c

  expect_identical(
    add_covariance_variable_terms(f, cs),
    a ~ b + c + USUBJID + COORDA + COORDB + GRP
  )
})
