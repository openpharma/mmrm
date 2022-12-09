# h_coef_table ----

test_that("h_coef_table works as expected", {
  object <- get_mmrm()
  result <- expect_silent(h_coef_table(object))
  expect_matrix(
    result,
    mode = "numeric"
  )
  expect_names(rownames(result), identical.to = names(coef(object)))
  expect_names(
    colnames(result),
    identical.to = c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")
  )
})

test_that("h_coef_table works as expected", {
  object <- get_mmrm_rank_deficient()
  result <- expect_silent(h_coef_table(object))
  expect_matrix(
    result,
    mode = "numeric",
    nrows = length(component(object, "beta_est_complete"))
  )
  expect_true(all(is.na(result["SEX2Female", ])))
})

# summary ----

test_that("summary works as expected", {
  object <- get_mmrm()
  result <- expect_silent(summary(object))
  expect_list(result)
  expect_named(
    result,
    c(
      "cov_type", "reml", "n_groups", "n_theta", "n_subjects", "n_timepoints", "n_obs",
      "beta_vcov", "varcor", "method", "coefficients", "n_singular_coefs", "aic_list", "call"
    )
  )
})

# h_print_call ----

test_that("h_print_call works as expected", {
  object <- get_mmrm()
  expect_snapshot_output(h_print_call(object$call, 1, 2, 3), cran = TRUE)
})

test_that("h_print_call works as expected for weighted fits", {
  object <- get_mmrm_weighted()
  expect_snapshot_output(h_print_call(object$call, 1, 2, 3), cran = TRUE)
})

# h_print_cov ----

test_that("h_print_cov works as expected", {
  object <- get_mmrm()
  expect_snapshot_output(h_print_cov("toep", 3, 1L), cran = TRUE)
  expect_snapshot_output(h_print_cov("toep", 6, 2L), cran = TRUE)
})

# h_print_aic_list ----

test_that("h_print_aic_list works as expected", {
  expect_snapshot_output(
    h_print_aic_list(
      list(AIC = 234.234235, BIC = 234.23, logLik = -252.234234, deviance = 345235.2323)
    ),
    cran = FALSE
  )
})

# print.summary.mmrm ----

test_that("print.summary.mmrm works as expected", {
  object <- get_mmrm()
  result <- summary(object)
  expect_snapshot_output(print(result, digits = 1), cran = FALSE)
})

test_that("print.summary.mmrm works as expected for weighted models", {
  object <- get_mmrm_weighted()
  result <- summary(object)
  expect_snapshot_output(print(result, digits = 1), cran = FALSE)
})

test_that("print.summary.mmrm works as expected for rank deficient fits", {
  object <- get_mmrm_rank_deficient()
  result <- summary(object)
  expect_snapshot_output(print(result, digits = 1), cran = FALSE)
})

test_that("print.summary.mmrm works as expected for grouped fits", {
  object <- get_mmrm_group()
  result <- summary(object)
  expect_snapshot_output(print(result, digits = 1), cran = FALSE)
})
# for spatial covariance matrix, covariance matrix of time points
# of unit distance will be displayed.
test_that("print.summary.mmrm works as expected for spatial fits", {
  object <- get_mmrm_spatial()
  result <- summary(object)
  expect_snapshot_output(print(result, digits = 1), cran = FALSE)
})
