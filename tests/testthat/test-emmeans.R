# recover_data ----

test_that("recover_data method works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- emmeans::recover_data(fit)
  expect_data_frame(result, nrows = nrow(fit$tmb_data$x_matrix))
  expect_named(
    attributes(result),
    c("names", "row.names", "class", "call", "terms", "predictors", "responses")
  )
})

# emm_basis ----

test_that("emm_basis method works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  trms <- stats::delete.response(stats::terms(fit$formula_parts$model_formula))
  xlev <- list(
    AVISIT = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ARMCD = c("PBO", "TRT"),
    RACE = c("Asian", "Black or African American", "White"),
    SEX = c("Male", "Female")
  )
  # Fit a linear model with the same formula so that we don't rely on
  # emm_basis just yet.
  lm_mod <- stats::lm(fit$formula_parts$model_formula, data = fev_data)
  grid <- emmeans::ref_grid(lm_mod)

  result <- emmeans::emm_basis(fit, trms = trms, xlev = xlev, grid = grid)
  expect_list(result)
  expect_named(result, c("X", "bhat", "nbasis", "V", "dffun", "dfargs"))
})

# emmeans ----

test_that("emmeans works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT))
  expect_class(result, "emmGrid")
  result_emmeans_rounded <- round(as.data.frame(result)$emmean, 1)
  expected_emmeans_rounded <- c(33.3, 37.1, 38.2, 41.9, 43.7, 46.8, 48.4, 52.8)
  expect_equal(result_emmeans_rounded, expected_emmeans_rounded, tolerance = 1e-4)
})

test_that("emmeans gives values close to what is expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT))
  result_df <- as.data.frame(result)

  # See design/SAS/sas_log_reml.txt for the source of numbers.
  expected_df <- data.frame(
    ARMCD = factor(rep(c("PBO", "TRT"), 4)),
    AVISIT = factor(rep(paste0("VIS", 1:4), each = 2)),
    emmean = c(33.3318, 37.1063, 38.1715, 41.9037, 43.6740, 46.7546, 48.3855, 52.7841),
    SE = c(0.7554, 0.7626, 0.6117, 0.6023, 0.4617, 0.5086, 1.1886, 1.1877),
    df = c(148, 143, 147, 144, 130, 130, 134, 133)
  )
  expect_identical(result_df[1:2], expected_df[1:2])
  expect_equal(result_df$emmean, expected_df$emmean, tolerance = 1e-4)
  expect_equal(result_df$SE, expected_df$SE, tolerance = 1e-4)
  expect_true(all(abs(result_df$df - expected_df$df) < 0.6))
})
