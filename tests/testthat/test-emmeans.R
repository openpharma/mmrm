# recover_data ----

test_that("recover_data method works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm()
  result <- emmeans::recover_data(fit)
  expect_data_frame(result, nrows = nrow(fit$tmb_data$x_matrix))
  expect_named(
    attributes(result),
    c("names", "row.names", "class", "call", "terms", "predictors", "responses")
  )
})

test_that("recover_data method works as expected for rank deficient model", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_rank_deficient()
  result <- emmeans::recover_data(fit)
  expect_data_frame(result, nrows = nrow(fit$tmb_data$x_matrix))
  expect_named(result, c("RACE", "SEX", "SEX2", "ARMCD", "AVISIT"))
})

# emm_basis ----

test_that("emm_basis method works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm()
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

test_that("emm_basis method works also for rank deficient fit", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_rank_deficient()
  trms <- stats::delete.response(stats::terms(fit$formula_parts$model_formula))
  xlev <- list(
    AVISIT = c("VIS1", "VIS2", "VIS3", "VIS4"),
    ARMCD = c("PBO", "TRT"),
    RACE = c("Asian", "Black or African American", "White"),
    SEX = c("Male", "Female"),
    SEX2 = c("Male", "Female")
  )
  # Fit a linear model with the same formula so that we don't rely on
  # emm_basis just yet.
  lm_mod <- stats::lm(fit$formula_parts$model_formula, data = fit$data)
  grid <- suppressMessages(emmeans::ref_grid(lm_mod))

  result <- emmeans::emm_basis(fit, trms = trms, xlev = xlev, grid = grid)
  expect_list(result)
  expect_named(result, c("X", "bhat", "nbasis", "V", "dffun", "dfargs"))
})

# emmeans ----

test_that("emmeans works as expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm()
  result <- expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT))
  expect_class(result, "emmGrid")
  result_emmeans <- as.data.frame(result)$emmean
  expected_emmeans <- c(33.3, 37.1, 38.2, 41.9, 43.7, 46.8, 48.4, 52.8)
  expect_equal(result_emmeans, expected_emmeans, tolerance = 1e-3)
})

test_that("emmeans gives values close to what is expected", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm()
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

test_that("emmeans works as expected also for rank deficient fit when singular coefficients are not involved", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_rank_deficient()
  expect_message(
    result <- emmeans::emmeans(fit, ~ ARMCD | AVISIT),
    "A nesting structure was detected in the fitted model"
  )
  expect_class(result, "emmGrid")
  result_emmeans <- as.data.frame(result)$emmean
  expected_emmeans <- c(33.3, 37.1, 38.2, 41.9, 43.7, 46.8, 48.4, 52.8)
  expect_equal(result_emmeans, expected_emmeans, tolerance = 1e-3)
})

test_that("emmeans works as expected also for rank deficient fit when singular coefficients are involved", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm_rank_deficient()
  # We get a message from `emmeans` but that is ok.
  expect_message(
    result <- emmeans::emmeans(fit, ~SEX2),
    "A nesting structure was detected in the fitted model"
  )
  expect_class(result, "emmGrid")
  result_emmeans <- as.data.frame(result)$emmean
  expect_message(expected <- emmeans::emmeans(fit, ~SEX))
  expected_emmeans <- as.data.frame(expected)$emmean
  expect_identical(result_emmeans, expected_emmeans)
})

test_that("emmeans works as expected also for weighted model", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  fit <- mmrm(formula, fev_data, weights = fev_data$WEIGHT)
  result <- expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT))
  expect_class(result, "emmGrid")
  result_emmeans_df <- as.data.frame(result)[, c("ARMCD", "AVISIT", "emmean", "SE")]

  # See design/SAS/sas_weighted.txt for the source of numbers.
  expected_emmeans_df <- data.frame(
    ARMCD = factor(c("PBO", "TRT", "PBO", "TRT", "PBO", "TRT", "PBO", "TRT")),
    AVISIT = factor(c("VIS1", "VIS1", "VIS2", "VIS2", "VIS3", "VIS3", "VIS4", "VIS4")),
    emmean = c(33.37, 36.77, 38.24, 41.66, 43.42, 46.82, 48.17, 52.43),
    SE = c(0.772, 0.776, 0.617, 0.585, 0.468, 0.543, 1.163, 1.226)
  )
  expect_equal(result_emmeans_df, expected_emmeans_df, tolerance = 1e-3)
})
