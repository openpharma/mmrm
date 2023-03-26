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

test_that("coef works as expected for rank deficient model", {
  object <- get_mmrm_tmb_rank_deficient()
  result <- expect_silent(coef(object))
  expected <- c(
    "(Intercept)" = 42.8058,
    SEXFemale = 0.0452,
    SEX2Female = NA
  )
  expect_equal(result, expected, tolerance = 1e-4)

  result2 <- expect_silent(coef(object, complete = FALSE))
  expected2 <- expected[1:2]
  expect_equal(result2, expected2, tolerance = 1e-4)
})

# fitted ----

test_that("fitted works as expected", {
  object <- get_mmrm_tmb()
  result <- expect_silent(fitted(object))
  expect_numeric(result, names = "unique", len = length(object$tmb_data$y_vector))
})

# predict -----

test_that("predict works for old patient, new visit", {
  # restrict patient one PT1 data to first visit
  data <- fev_data[!(fev_data$USUBJID == "PT1" &
      fev_data$AVISIT %in% c("VIS2", "VIS3", "VIS4")), ]
  # fit model
  fit <- fit_mmrm(.tmb_formula, fev_data, weights = rep(1, nrow(fev_data)))
  # predict the hold out
  newdata <- fev_data[fev_data$USUBJID == "PT1" &
      fev_data$AVISIT %in% c("VIS2", "VIS3", "VIS4"), ]
  result <- expect_silent(predict(fit, newdata))
})

# model.frame ----

test_that("model.frame works as expected with defaults", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works as expected with excludes", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object, exclude = c("subject_var", "visit_var")))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame returns full model frame if requested", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object, exclude = NULL))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE", "USUBJID", "AVISIT", "(weights)"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works if variable transformed", {
  fit1 <- get_mmrm_transformed()
  result <- expect_silent(model.frame(fit1))
  expect_data_frame(result, nrows = length(fit1$tmb_data$y_vector))
  expect_named(result, c("FEV1", "log(FEV1_BL)", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works for new data", {
  fit1 <- get_mmrm_transformed()
  result <- expect_silent(model.frame(fit1, data = fev_data[complete.cases(fev_data), ][1:20, ]))
  expect_data_frame(result, nrows = 20L)
  expect_named(result, c("FEV1", "log(FEV1_BL)", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works regardless of na.action settings", {
  na_action <- getOption("na.action")
  fit1 <- get_mmrm_transformed()
  expect_warning(
    model.frame(fit1, na.action = "na.fail"),
    "na.action is always set to `na.omit` for `mmrm`!"
  )
  options(na.action = "na.fail")
  expect_warning(
    model.frame(fit1),
    "na.action is always set to `na.omit` for `mmrm`!"
  )
  on.exit(options(na.action = na_action))
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

test_that("vcov works as expected for rank deficient model", {
  object <- get_mmrm_tmb_rank_deficient()
  result <- expect_silent(vcov(object))
  expect_matrix(result, mode = "numeric")
  nms <- c("(Intercept)", "SEXFemale", "SEX2Female")
  expect_names(rownames(result), identical.to = nms)
  expect_names(colnames(result), identical.to = nms)

  result2 <- expect_silent(vcov(object, complete = FALSE))
  nms2 <- c("(Intercept)", "SEXFemale")
  expect_names(rownames(result2), identical.to = nms2)
  expect_names(colnames(result2), identical.to = nms2)
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

test_that("print.mmrm_tmb works as expected for rank deficient fits", {
  object_mmrm_tmb <- get_mmrm_tmb_rank_deficient()
  expect_snapshot_output(print(object_mmrm_tmb), cran = FALSE)
})

# residuals.mmrm_tmb ----

test_that("residuals works as expected", {
  object <- get_mmrm()

  result_resp <- expect_silent(residuals(object, type = "response"))
  expect_double(result_resp, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_resp, 5), c(-1.2349, -31.6025, -4.1618, -4.2406, 2.9770), tolerance = 1e-4)

  result_pearson <- expect_silent(residuals(object, type = "pearson"))
  expect_double(result_pearson, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_pearson, 5), c(-0.23957, -3.23296, -0.80740, -1.09871, 0.30455), tolerance = 1e-4)

  result_norm <- expect_silent(residuals(object, type = "normalized"))
  expect_double(result_norm, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_norm, 5), c(-0.23957, -3.23322, -0.80740, -0.99548, 0.43232), tolerance = 1e-4)
})

test_that("residuals works as expected with grouped covariance structure", {
  object <- get_mmrm_group()

  result_resp <- expect_silent(residuals(object, type = "response"))
  expect_double(result_resp, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_resp, 5), c(-4.7084, -24.1957, -9.6965, -4.2728, 7.6564), tolerance = 1e-4)

  result_pearson <- expect_silent(residuals(object, type = "pearson"))
  expect_double(result_pearson, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_pearson, 5), c(-0.73835, -1.83991, -1.53172, -0.88145, 0.66653), tolerance = 1e-4)

  result_norm <- expect_silent(residuals(object, type = "normalized"))
  expect_double(result_norm, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_norm, 5), c(-0.73835, -1.88475, -1.53172, -1.02026, 0.54335), tolerance = 1e-4)
})

test_that("residuals works as expected with weighted model fit", {
  object <- get_mmrm_weighted()

  result_resp <- expect_silent(residuals(object, type = "response"))
  expect_double(result_resp, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_resp, 5), c(-1.3356, -31.6028, -3.7467, -3.9470, 2.8818), tolerance = 1e-4)

  result_pearson <- expect_silent(residuals(object, type = "pearson"))
  expect_double(result_pearson, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_pearson, 5), c(-0.29917, -4.07046, -0.51393, -1.01538, 0.37118), tolerance = 1e-4)

  result_norm <- expect_silent(residuals(object, type = "normalized"))
  expect_double(result_norm, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_norm, 5), c(-0.29917, -4.07727, -0.51393, -0.96419, 0.45466), tolerance = 1e-4)
})

test_that("residuals works as expected with a model using spatial covariance structure", {
  object <- get_mmrm_spatial()

  result_resp <- expect_silent(residuals(object, type = "response"))
  expect_double(result_resp, len = length(object$tmb_data$y_vector))
  expect_equal(head(result_resp, 5), c(-4.5428, -24.0301, -8.8329, -3.4092, 8.5200), tolerance = 1e-4)

  expect_error(residuals(object, type = "pearson"))
  expect_error(residuals(object, type = "normalized"))
})

test_that("pearson residuals helper function works as expected", {
  object <- get_mmrm()
  resid_response <- residuals(object, type = "response")

  result_pearson <- h_residuals_pearson(object, resid_response)
  expect_double(result_pearson, len = length(object$tmb_data$y_vector))
  expect_equal(tail(result_pearson, 5), c(2.22057, 1.79050, 0.53322, 0.87243, 0.70477), tolerance = 1e-4)
})

test_that("normalized residuals helper function works as expected", {
  object <- get_mmrm()
  resid_response <- residuals(object, type = "response")

  result_norm <- h_residuals_normalized(object, resid_response)
  expect_double(result_norm, len = length(object$tmb_data$y_vector))
  expect_equal(tail(result_norm, 5), c(1.99092, 1.49689, 0.53322, 0.71055, 0.56152), tolerance = 1e-4)
})
