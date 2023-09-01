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

# predict ----

test_that("parsnip works with predict", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  model <- parsnip::linear_reg() |>
    parsnip::set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
    parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  result <- expect_silent(predict(model, new_data = fev_data))
  expect_tibble(result)
  expect_names(names(result), identical.to = ".pred")
})

test_that("parsnip allows to pass additional arguments to predict with type `raw` via `opts`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  model <- parsnip::linear_reg() |>
    parsnip::set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
    parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  result <- expect_silent(predict(
    model,
    new_data = fev_data,
    type = "raw",
    opts = list(
      interval = "prediction",
      nsim = 10L,
      se.fit = TRUE
    )
  ))
  expect_matrix(result)
  expect_names(colnames(result), identical.to = c("fit", "se", "lwr", "upr"))
  expect_true(all(result[is.na(fev_data$FEV1), "se"] > 0))
})

# augment ----

test_that("parsnip works with augment and using `new_data`", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0")

  model <- parsnip::linear_reg() |>
    parsnip::set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
    parsnip::fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  result <- expect_silent(augment(model, new_data = fev_data))
  expect_tibble(result)
  expect_names(names(result), must.include = c(".pred", ".resid"))
})
