# recover_data ----

test_that("emmeans emits a message about mmrm registration on load", {
  # Don't use skip_if_not_installed yet since it runs requireNamespace, which
  # will itself load the package without testing startup.
  skip_if(length(find.package("emmeans")) < 1)

  # Detach emmeans in case it was loaded in session or previous test run
  if (isNamespaceLoaded("emmeans")) {
    unloadNamespace("emmeans")
  }

  expect_message(library(emmeans), "mmrm")
})

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
  skip_if_r_devel_linux_clang()
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- get_mmrm()
  result <- expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT))
  expect_class(result, "emmGrid")
  result_emmeans <- as.data.frame(result)$emmean
  expected_emmeans <- c(33.3, 37.1, 38.2, 41.9, 43.7, 46.8, 48.4, 52.8)
  expect_equal(result_emmeans, expected_emmeans, tolerance = 1e-3)
})

test_that("emmeans works as expected for transformed variables", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(FEV1 ~ log(FEV1_BL) + AVISIT + ar1(AVISIT | USUBJID), data = fev_data)
  result <- expect_silent(emmeans::emmeans(fit, ~AVISIT))
  expect_class(result, "emmGrid")
  result_emmeans <- as.data.frame(result)$emmean
  expected_emmeans <- c(34.99641, 39.86572, 44.54411, 50.53663)
  expect_equal(result_emmeans, expected_emmeans, tolerance = 1e-3)
})

test_that("emmeans works as expected for transformed variables and fixed effect is not visit", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  fit <- mmrm(FEV1 ~ log(FEV1_BL) + ARMCD + ar1(AVISIT | USUBJID), data = fev_data)
  result <- expect_silent(emmeans::emmeans(fit, ~ ARMCD | AVISIT))
  expect_class(result, "emmGrid")
  result_emmeans <- as.data.frame(result)$emmean
  expected_emmeans <- c(40.41980, 44.78855, 40.41980, 44.78855, 40.41980, 44.78855, 40.41980, 44.78855)
  expect_equal(result_emmeans, expected_emmeans, tolerance = 1e-3)
})

test_that("emmeans gives values close to what is expected", {
  skip_if_r_devel_linux_clang()
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
  skip_if_r_devel_linux_clang()
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

test_that("emmeans gives d.f. close to what is expected for weighted model", {
  skip_if_not_installed("emmeans", minimum_version = "1.6")

  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  fev_data <- within(fev_data, AVISIT <- relevel(AVISIT, ref = "VIS4")) # nolint
  fit <- mmrm(formula, fev_data, weights = fev_data$WEIGHT)
  result <- expect_silent(emmeans::emmeans(fit, pairwise ~ ARMCD * AVISIT))
  expect_class(result, "emm_list")
  result_emmeans_df <- as.data.frame(result$emmeans)[, c("ARMCD", "AVISIT", "df")]
  result_contrasts_df <- as.data.frame(result$contrasts)[, c("contrast", "df")]

  # See design/SAS/R_weighted_nlme.txt for the source of numbers.
  expected_emmeans_df <- data.frame(
    ARMCD = factor(c("PBO", "TRT", "PBO", "TRT", "PBO", "TRT", "PBO", "TRT")),
    AVISIT = factor(c("VIS4", "VIS4", "VIS1", "VIS1", "VIS2", "VIS2", "VIS3", "VIS3"),
      levels = c("VIS4", "VIS1", "VIS2", "VIS3")
    ),
    df = c(134, 132, 147, 142, 144, 143, 128, 128)
  )
  expect_equal(result_emmeans_df, expected_emmeans_df, tolerance = 1e-2)

  expected_contrasts_df <- data.frame(
    contrast = c(
      "PBO VIS4 - TRT VIS4", "PBO VIS4 - PBO VIS1", "PBO VIS4 - TRT VIS1",
      "PBO VIS4 - PBO VIS2", "PBO VIS4 - TRT VIS2", "PBO VIS4 - PBO VIS3",
      "PBO VIS4 - TRT VIS3", "TRT VIS4 - PBO VIS1", "TRT VIS4 - TRT VIS1",
      "TRT VIS4 - PBO VIS2", "TRT VIS4 - TRT VIS2", "TRT VIS4 - PBO VIS3",
      "TRT VIS4 - TRT VIS3", "PBO VIS1 - TRT VIS1", "PBO VIS1 - PBO VIS2",
      "PBO VIS1 - TRT VIS2", "PBO VIS1 - PBO VIS3", "PBO VIS1 - TRT VIS3",
      "TRT VIS1 - PBO VIS2", "TRT VIS1 - TRT VIS2", "TRT VIS1 - PBO VIS3",
      "TRT VIS1 - TRT VIS3", "PBO VIS2 - TRT VIS2", "PBO VIS2 - PBO VIS3",
      "PBO VIS2 - TRT VIS3", "TRT VIS2 - PBO VIS3", "TRT VIS2 - TRT VIS3",
      "PBO VIS3 - TRT VIS3"
    ),
    df = c(
      133, 148, 231, 158, 198, 157, 190, 223, 122, 196, 147, 170, 161, 142,
      151, 257, 157, 251, 258, 135, 225, 168, 144, 160, 270, 264, 159, 129
    )
  )
  expect_equal(result_contrasts_df, expected_contrasts_df, tolerance = 1e-2)
})
