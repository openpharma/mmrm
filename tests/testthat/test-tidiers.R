# glance ----

object <- get_mmrm()

test_that("glance returns expected values in summary in tibble form", {
  result <- object |> glance()
  expected <- summary(object)$aic_list
  expect_true(inherits(result,'tbl'))
  expect_equal(as.list(result), expected, tolerance = 1e-4)
})

# tidy ----

test_that("tidy returns expected values in summary in tibble form", {
  result <- object |> tidy()
  expected <- summary(object)$coef
  expect_true(inherits(result,'tbl'))

  estimate_result <- as.numeric(result[[2]])
  estimate_expected <- as.numeric(expected[,1])
  expect_equal(estimate_result, estimate_expected)
})

test_that("tidy conf.int", {
  result <- object |> tidy(conf.int = TRUE)
  result <- result[,c(1,7,8)]
  expected <- mmrm:::h_mmrm_confint_terms(object)
  expected <- expected[order(expected$term),]
  expect_true(inherits(result,'tbl'))
  expect_equal(result,expected)
})

test_that("tidy conf.int level", {
  result <- object |> tidy(conf.int = TRUE, conf.level = 0.9)
  result <- result[,c(1,7,8)]
  expected <- mmrm:::h_mmrm_confint_terms(object, level = 0.9)
  expected <- expected[order(expected$term),]
  expect_true(inherits(result,'tbl'))
  expect_equal(result,expected)
})

# augment ----

test_that("augment returns tibble form", {
  result <- object |> augment()
  expect_true(inherits(result,'tbl'))
})

test_that("augment returns prediction", {
  result <- object |> augment(newdata = object$data)
  expected <- object |> predict(newdata = object$data) |> as.numeric()
  expect_equal(result$.fitted,expected)
})

test_that("augment returns prediction with confidence interval", {
  result <- object |> augment(newdata = object$data, interval = 'confidence')
  result_ci <- result[,c('.fitted','.se.fit','.lower','.upper')]
  expected <- object |> predict(newdata = object$data, se.fit = TRUE, interval = 'confidence') |> tibble::as_tibble()
  names(expected) <- c('.fitted','.se.fit','.lower','.upper')
  expect_equal(result_ci,expected)
})

test_that("augment returns prediction with confidence interval with level", {
  result <- object |> augment(newdata = object$data, interval = 'confidence', level = 0.9)
  result_ci <- result[,c('.fitted','.se.fit','.lower','.upper')]
  expected <- object |> predict(newdata = object$data, se.fit = TRUE, interval = 'confidence', level = 0.9) |> tibble::as_tibble()
  names(expected) <- c('.fitted','.se.fit','.lower','.upper')
  expect_equal(result_ci,expected)
})

test_that("augment returns prediction with prediction interval", {
  set.seed(132)
  result <- object |> augment(newdata = object$data, interval = 'prediction')
  result_pi <- result[,c('.fitted','.se.fit','.lower','.upper')]
  set.seed(132)
  expected <- object |> predict(newdata = object$data, se.fit = TRUE, interval = 'prediction') |> tibble::as_tibble()
  names(expected) <- c('.fitted','.se.fit','.lower','.upper')
  expect_equal(result_pi,expected)
})

test_that("augment returns prediction with prediction interval with level", {
  set.seed(132)
  result <- object |> augment(newdata = object$data, interval = 'prediction', level = 0.9)
  result_pi <- result[,c('.fitted','.se.fit','.lower','.upper')]
  set.seed(132)
  expected <- object |> predict(newdata = object$data, se.fit = TRUE, interval = 'prediction', level = 0.9) |> tibble::as_tibble()
  names(expected) <- c('.fitted','.se.fit','.lower','.upper')
  expect_equal(result_pi,expected)
})

test_that("augment returns residuals response", {
  result <- object |> augment(type.residuals = 'response')
  result_resid <- result$.resid
  expected <- object |> residuals(type = 'response')
  expect_equal(result_resid,expected)
})

test_that("augment returns residuals pearson", {
  result <- object |> augment(type.residuals = 'pearson')
  result_resid <- result$.resid
  expected <- object |> residuals(type = 'pearson')
  expect_equal(result_resid,expected)
})

test_that("augment returns residuals normalized", {
  result <- object |> augment(type.residuals = 'normalized')
  result_resid <- result$.resid
  expected <- object |> residuals(type = 'normalized')
  expect_equal(result_resid,expected)
})
