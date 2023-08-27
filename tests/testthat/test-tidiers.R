# glance ----

test_that("glance returns expected values in summary in tibble form", {
  object <- get_mmrm()
  result <- object |> glance()
  expected <- summary(object)$aic_list
  expect_s3_class(result, "tbl")
  expect_equal(as.list(result), expected, tolerance = 1e-4)
})

# tidy ----

test_that("tidy returns expected values in summary in tibble form", {
  object <- get_mmrm()
  result <- object |> tidy()
  expected <- summary(object)$coef
  expect_s3_class(result, "tbl")

  estimate_result <- as.numeric(result[[2]])
  estimate_expected <- as.numeric(expected[, 1])
  expect_equal(estimate_result, estimate_expected)
})

test_that("tidy conf.int", {
  object <- get_mmrm()
  result <- tidy(object, conf.int = TRUE)
  result <- result[, c(1, 7, 8)]
  expected <- h_mmrm_confint_terms(object)
  expected <- expected[order(expected$term), ]
  expect_true(inherits(result, "tbl"))
  expect_equal(result, expected)
})

test_that("tidy conf.int level", {
  object <- get_mmrm()
  result <- tidy(object, conf.int = TRUE, conf.level = 0.9)
  result <- result[, c(1, 7, 8)]
  expected <- mmrm:::h_mmrm_confint_terms(object, level = 0.9)
  expected <- expected[order(expected$term), ]
  expect_s3_class(result, "tbl")
  expect_equal(result, expected)
})

# augment ----

test_that("augment returns tibble form", {
  object <- get_mmrm()
  result <- augment(object)
  expect_s3_class(result, "tbl")
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
  expected <- tibble::as_tibble(predict(object, newdata = object$data, se.fit = TRUE, interval = "confidence"))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_ci, expected)
})

test_that("augment returns prediction with confidence interval with level", {
  object <- get_mmrm()
  result <- augment(object, newdata = object$data, interval = "confidence", level = 0.9)
  result_ci <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  expected <- tibble::as_tibble(predict(object, newdata = object$data, se.fit = TRUE, interval = "confidence", level = 0.9))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_ci, expected)
})

test_that("augment returns prediction with prediction interval", {
  object <- get_mmrm()
  set.seed(132)
  result <- augment(object, newdata = object$data, interval = "prediction")
  result_pi <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  set.seed(132)
  expected <- tibble::as_tibble(predict(object, newdata = object$data, se.fit = TRUE, interval = "prediction"))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_pi, expected)
})

test_that("augment returns prediction with prediction interval with level", {
  object <- get_mmrm()
  set.seed(132)
  result <- augment(object, newdata = object$data, interval = "prediction", level = 0.9)
  result_pi <- result[, c(".fitted", ".se.fit", ".lower", ".upper")]
  set.seed(132)
  expected <- tibble::as_tibble(predict(object, newdata = object$data, se.fit = TRUE, interval = "prediction", level = 0.9))
  names(expected) <- c(".fitted", ".se.fit", ".lower", ".upper")
  expect_equal(result_pi, expected)
})

test_that("augment returns residuals response", {
  object <- get_mmrm()
  result <- augment(object, type.residuals = "response")
  result_resid <- result$resid
  expected <- residuals(object, type = "response")
  expect_equal(result_resid, expected)
})

test_that("augment returns residuals pearson", {
  object <- get_mmrm()
  object <- get_mmrm()
  result <- augment(object, type.residuals = "pearson")
  result_resid <- result$resid
  expected <- residuals(object, type = "pearson")
  expect_equal(result_resid, expected)
})

test_that("augment returns residuals normalized", {
  object <- get_mmrm()
  result <- augment(object, type.residuals = "normalized")
  result_resid <- result$resid
  expected <- residuals(object, type = "normalized")
  expect_equal(result_resid, expected)
})

# h_mmrm_confint_terms ----

test_that("h_mmrm_confint_terms works as expected", {
  result <- expect_silent(h_mmrm_confint_terms())
  expected <-
  expect_identical(result, expected)
})

# h_mmrm_augment_newdata ----

test_that("h_mmrm_augment_newdata works as expected", {
  result <- expect_silent(h_mmrm_augment_newdata())
  expected <-
  expect_identical(result, expected)
})

# h_augment_tibble ----

test_that("h_augment_tibble works as expected", {
  result <- expect_silent(h_augment_tibble())
  expected <-
  expect_identical(result, expected)
})
