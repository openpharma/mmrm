test_that("mmrm works when using tidymodels", {
  library(tidymodels)
  expect_silent(
    linear_reg() |>
      set_engine("mmrm", control = mmrm_control(method = "Satterthwaite"), covariance = ~ us(AVISIT | USUBJID)) |>
      fit(FEV1 ~ RACE + ARMCD * AVISIT, fev_data)
  )
  expect_silent(
    linear_reg() |>
      set_engine(
        "mmrm",
        control = mmrm_control(method = "Satterthwaite"),
        covariance = as.cov_struct(~ us(AVISIT | USUBJID))
      ) |>
      fit(FEV1 ~ RACE + ARMCD * AVISIT, fev_data)
  )
  expect_silent(
    linear_reg() |>
      set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
      fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
  )
})
