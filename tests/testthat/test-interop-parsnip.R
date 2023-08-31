# Needed for the parsnip calls to work below, more lightweight than loading tidymodels:
library(generics)

# linear_reg, set_engine, fit ----

test_that("parsnip works with a formula for the `covariance` specification outside of `fit()`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  result <- expect_silent(
    parsnip::linear_reg() |>
      parsnip::set_engine(
        "mmrm",
        control = mmrm_control(method = "Satterthwaite"),
        covariance = ~ us(AVISIT | USUBJID)
      ) |>
      parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT, fev_data)
  )
  expect_s3_class(result, "model_fit")
})

test_that("parsnip works with a `as.cov_struct` for the `covariance` specification outside of `fit()`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  result <- expect_silent(
    parsnip::linear_reg() |>
      parsnip::set_engine(
        "mmrm",
        control = mmrm_control(method = "Satterthwaite"),
        covariance = as.cov_struct(~ us(AVISIT | USUBJID))
      ) |>
      parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT, fev_data)
  )
  expect_s3_class(result, "model_fit")
})

test_that("parsnip works with the covariance structure in the formula in `fit()`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  result <- expect_silent(
    parsnip::linear_reg() |>
      parsnip::set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
      parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  )
  expect_s3_class(result, "model_fit")
})

# augment ----

test_that("parsnip works with augment without providing `new_data`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  model <- parsnip::linear_reg() |>
      parsnip::set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
      parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  result <- expect_silent(augment(model))
})

test_that("parsnip works with augment and using `new_data`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  model <- parsnip::linear_reg() |>
      parsnip::set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
      parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  result <- expect_silent(augment(model, new_data = fev_data))
})
