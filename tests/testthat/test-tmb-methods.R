# coef ----

test_that("coef works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(coef(object))
  expected <- c(
    "(Intercept)" = 41.2273,
    "RACEBlack or African American" = 0.8002,
    "RACEWhite" = 5.8791
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

# fitted ----

test_that("fitted works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(fitted(object))
  expect_numeric(result, names = "unique", len = length(object$tmb_data$y_vector))
})

# model.frame ----

test_that("model.frame works as expected with defaults", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame returns full model frame if requested", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object, full = TRUE))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE", "USUBJID", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

# logLik ----

test_that("logLik works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(logLik(object))
  expected <- -1821.19736
  expect_equal(result, expected)
})

# formula ----

test_that("formula works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(formula(object))
  expected <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  expect_false(identical(environment(result), environment(expected)))
  expect_equal(result, expected, ignore_attr = TRUE)
})

# vcov ----

test_that("vcov works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(vcov(object))
  expect_matrix(result, mode = "numeric")
  nms <- c("(Intercept)", "RACEBlack or African American", "RACEWhite")
  expect_names(rownames(result), identical.to = nms)
  expect_names(colnames(result), identical.to = nms)
})

# VarCorr ----

test_that("VarCorr works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(VarCorr(object))
  expect_matrix(result, mode = "numeric")
  nms <- c("VIS1", "VIS2", "VIS3", "VIS4")
  expect_names(rownames(result), identical.to = nms)
  expect_names(colnames(result), identical.to = nms)
})

# deviance ----

test_that("deviance works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(deviance(object))
  expected <- 3642.3947
  expect_equal(result, expected)
})

# AIC ----

test_that("AIC works as expected with defaults", {
  object <- get_mmrm_tmb()
  result <- expect_silent(AIC(object))
  expected <- -2 * logLik(object) + 2 * length(object$theta_est)
  expect_equal(result, expected)
})

test_that("AIC works as expected with different k", {
  object <- get_mmrm_tmb()
  result <- expect_silent(AIC(object, k = 5))
  expected <- -2 * logLik(object) + 5 * length(object$theta_est)
  expect_equal(result, expected)
})

test_that("corrected AIC works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(AIC(object, corrected = TRUE))
  m <- nrow(object$tmb_data$x_matrix) - ncol(object$tmb_data$x_matrix)
  n_theta <- length(object$theta_est)
  multiplier <- m / (m - n_theta - 1)
  expected <- -2 * logLik(object) + 2 * length(object$theta_est) * multiplier
  expect_equal(result, expected)
})

# BIC ----

test_that("BIC works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(BIC(object))
  expected <- -2 * logLik(object) + log(object$tmb_data$n_subjects) * length(object$theta_est)
  expect_equal(result, expected)
})

# print.mmrm_tmb ----

test_that("print.mmrm_tmb works as expected", {
  object_mmrm_tmb <- get_mmrm_tmb()
  expect_snapshot_output(print(object_mmrm_tmb), cran = FALSE)
  object_mmrm <- get_mmrm()
  expect_snapshot_output(print(object_mmrm), cran = FALSE)
})
