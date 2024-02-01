# glance ----

test_that("glance returns expected values in summary in tibble form", {
  object <- get_mmrm()
  result <- object |> glance()
  expected <- summary(object)$aic_list
  expect_tibble(result)
  expect_equal(as.list(result), expected, tolerance = 1e-4)
})

# tidy ----

test_that("tidy returns expected values in summary in tibble form", {
  object <- get_mmrm()
  result <- object |> tidy()
  expected <- summary(object)$coef
  expect_tibble(result)

  estimate_result <- as.numeric(result[[2]])
  estimate_expected <- as.numeric(expected[, 1])
  expect_equal(estimate_result, estimate_expected)
})

test_that("tidy conf.int", {
  object <- get_mmrm()
  result <- tidy(object, conf.int = TRUE)
  result <- result[, c(1, 7, 8)]
  expected <- h_tbl_confint_terms(object)
  expected <- expected[order(expected$term), ]
  expect_true(inherits(result, "tbl"))
  expect_equal(result, expected)
})

test_that("tidy conf.int level", {
  object <- get_mmrm()
  result <- tidy(object, conf.int = TRUE, conf.level = 0.9)
  result <- result[, c(1, 7, 8)]
  expected <- h_tbl_confint_terms(object, level = 0.9)
  expected <- expected[order(expected$term), ]
  expect_tibble(result)
  expect_equal(result, expected)
})

# augment ----

test_that("augment returns tibble form when called with default arguments", {
  object <- get_mmrm()
  result <- augment(object)
  expect_tibble(result)
})

test_that("augment returns prediction", {
  object <- get_mmrm()
  result <- augment(object, newdata = object$data)
  expected <- as.numeric(predict(object, newdata = object$data))
  expect_equal(result$.fitted, expected)
})

test_that("augment returns prediction with confidence interval", {
  object <- get_mmrm()
  result <- augment(object, newdata = object$data, interval = "confidence")
  result_ci <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  expected <- tibble::as_tibble(predict(
    object,
    newdata = object$data,
    se.fit = TRUE,
    interval = "confidence"
  ))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_ci, expected)
})

test_that("augment returns prediction with confidence interval with level", {
  object <- get_mmrm()
  result <- augment(object, newdata = object$data, interval = "confidence", level = 0.9)
  result_ci <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  expected <- tibble::as_tibble(predict(
    object,
    newdata = object$data,
    se.fit = TRUE,
    interval = "confidence",
    level = 0.9
  ))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_ci, expected)
})

test_that("augment returns prediction with prediction interval", {
  object <- get_mmrm()
  set.seed(132)
  result <- augment(object, newdata = object$data, interval = "prediction", nsim = 10L)
  result_pi <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  set.seed(132)
  expected <- tibble::as_tibble(predict(
    object,
    newdata = object$data,
    se.fit = TRUE,
    interval = "prediction",
    nsim = 10L
  ))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_pi, expected)
})

test_that("augment returns prediction with prediction interval with level", {
  object <- get_mmrm()
  set.seed(132)
  result <- augment(object, newdata = object$data, interval = "prediction", level = 0.9, nsim = 10L)
  result_pi <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  set.seed(132)
  expected <- tibble::as_tibble(predict(
    object,
    newdata = object$data,
    se.fit = TRUE,
    interval = "prediction",
    nsim = 10L,
    level = 0.9
  ))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_pi, expected)
})

test_that("augment returns residuals response", {
  object <- get_mmrm()
  result <- augment(object, type.residuals = "response")
  result_resid <- result$.resid
  expected <- residuals(object, type = "response")
  expect_equal(result_resid, expected)
})

test_that("augment returns residuals pearson", {
  object <- get_mmrm()
  result <- augment(object, type.residuals = "pearson")
  result_resid <- result$.resid
  expected <- residuals(object, type = "pearson")
  expect_equal(result_resid, expected)
})

test_that("augment returns residuals normalized", {
  object <- get_mmrm()
  result <- augment(object, type.residuals = "normalized")
  result_resid <- result$.resid
  expected <- residuals(object, type = "normalized")
  expect_equal(result_resid, expected)
})

# h_tbl_confint_terms ----

test_that("h_tbl_confint_terms works as expected", {
  object <- get_mmrm()
  result <- expect_silent(h_tbl_confint_terms(object))
  expect_tibble(result, nrows = 11, ncols = 3)
  expect_names(names(result), identical.to = c("term", "conf.low", "conf.high"))
})

test_that("h_tbl_confint_terms works for model with only intercept", {
  object <- mmrm(FEV1 ~ 1 + us(AVISIT | USUBJID), data = fev_data)
  result <- expect_silent(h_tbl_confint_terms(object))
  expect_tibble(result, nrows = 1, ncols = 3)
  expect_names(names(result), identical.to = c("term", "conf.low", "conf.high"))
  expect_identical(result$term[1L], "(Intercept)")
})

# h_newdata_add_pred ----

test_that("h_newdata_add_pred works as expected when se_fit and interval requested", {
  object <- get_mmrm()
  result <- expect_silent(h_newdata_add_pred(
    x = object,
    newdata = object$data,
    se_fit = TRUE,
    interval = "confidence"
  ))
  expect_tibble(result, nrows = nrow(object$data))
  expect_names(names(result), must.include = c(".fitted", ".lower", ".upper", ".se.fit"))
  expect_numeric(result$.fitted, any.missing = FALSE)
  expect_numeric(result$.lower, any.missing = FALSE)
  expect_numeric(result$.upper, any.missing = FALSE)
  expect_numeric(result$.se.fit, any.missing = FALSE)
})

test_that("h_newdata_add_pred fails when se_fit requested with interval none", {
  object <- get_mmrm()
  expect_error(
    h_newdata_add_pred(
      x = object,
      newdata = object$data,
      se_fit = TRUE,
      interval = "none"
    ),
    "Must be FALSE"
  )
})

test_that("h_newdata_add_pred works as expected when interval requested but not se_fit", {
  object <- get_mmrm()
  result <- expect_silent(h_newdata_add_pred(
    x = object,
    newdata = object$data,
    se_fit = FALSE,
    interval = "confidence"
  ))
  expect_tibble(result, nrows = nrow(object$data))
  expect_names(
    names(result),
    must.include = c(".fitted", ".lower", ".upper"),
    disjunct.from = ".se.fit"
  )
})

test_that("h_newdata_add_pred works when only fit requested", {
  object <- get_mmrm()
  result <- expect_silent(h_newdata_add_pred(
    x = object,
    newdata = object$data,
    se_fit = FALSE,
    interval = "none"
  ))
  expect_tibble(result, nrows = nrow(object$data))
  expect_names(
    names(result),
    must.include = c(".fitted"),
    disjunct.from = c(".se.fit", ".lower", ".upper")
  )
})

# h_df_to_tibble ----

test_that("h_df_to_tibble works as expected", {
  df <- data.frame(a = 5, b = 3)
  result <- expect_silent(h_df_to_tibble(df))
  expected <- tibble::tibble(a = 5, b = 3)
  expect_identical(result, expected)
})

test_that("h_df_to_tibble adds a .rownames column as expected", {
  df <- data.frame(a = 5, b = 3, row.names = "first")
  result <- expect_silent(h_df_to_tibble(df))
  expected <- tibble::tibble(.rownames = "first", a = 5, b = 3)
  expect_identical(result, expected)
})

test_that("h_df_to_tibble catches errors with a useful message", {
  not_df <- get_mmrm()
  expect_error(h_df_to_tibble(not_df), "Could not coerce data to `tibble`")
})
