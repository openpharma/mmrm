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
      "beta_vcov", "varcor", "method", "vcov", "coefficients", "n_singular_coefs", "aic_list", "call"
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

# confint ----

test_that("confint works for different significance levels", {
  object <- get_mmrm()
  expect_silent(confint(object, level = 1))
  expect_silent(confint(object, level = 0))
  expect_silent(confint(object, level = 0.5))
})

test_that("confint works for different `parm` input", {
  object <- get_mmrm()
  res <- expect_silent(confint(object, parm = c(1, 2, 3)))
  expect_identical(row.names(res), names(coef(object))[c(1, 2, 3)])

  res <- expect_silent(confint(object, parm = c("ARMCDTRT", "AVISITVIS4")))
  expect_identical(row.names(res), c("ARMCDTRT", "AVISITVIS4"))

  expect_error(
    confint(object, parm = 100),
    "Element 1 is not <= 11"
  )
  expect_error(
    confint(object, parm = c("a", "b")),
    "Must be a subset of"
  )
})

test_that("confint give same result as emmeans if no interaction term", {
  object <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data)
  emm <- emmeans(object, ~ARMCD)
  emm_pair <- pairs(emm, reverse = TRUE)
  conf <- confint(emm_pair)
  conf_coef <- confint(object, 2)
  expect_equal(conf$lower.CL, conf_coef[, 1])
  expect_equal(conf$upper.CL, conf_coef[, 2])
})

test_that("confint give same result as SAS on unstructured", {
  # SAS result file is "design/SAS/sas_coef_ci_ml_un.txt"
  object <- mmrm(FEV1 ~ ARMCD + SEX + us(AVISIT | USUBJID), data = fev_data, reml = FALSE)
  conf_coef <- confint(object)
  cis <- matrix(
    c(40.0071, 42.1609, 2.5714, 5.0838, -1.3968, 1.1204),
    ncol = 2, byrow = TRUE
  )
  expect_equal(conf_coef, cis, ignore_attr = TRUE, tolerance = 1e-4)

  # SAS result file is "design/SAS/sas_coef_ci_reml_un.txt"
  object <- mmrm(FEV1 ~ ARMCD + SEX + us(AVISIT | USUBJID), data = fev_data, reml = TRUE)
  conf_coef <- confint(object)
  cis <- matrix(
    c(39.9890, 42.1634, 2.5584, 5.0945, -1.4108, 1.1301),
    ncol = 2, byrow = TRUE
  )
  expect_equal(conf_coef, cis, ignore_attr = TRUE, tolerance = 1e-4)
})

test_that("confint give same result as SAS on ar1", {
  # SAS result file is "design/SAS/sas_coef_ci_ml_ar1.txt"
  object <- mmrm(FEV1 ~ ARMCD + SEX + ar1(AVISIT | USUBJID), data = fev_data, reml = FALSE)
  conf_coef <- confint(object)
  cis <- matrix(
    c(38.5271, 41.8057, 2.3353, 6.1039, -1.6559, 2.1193),
    ncol = 2, byrow = TRUE
  )
  expect_equal(conf_coef, cis, ignore_attr = TRUE, tolerance = 1e-4)

  # SAS result file is "design/SAS/sas_coef_ci_reml_ar1.txt"
  object <- mmrm(FEV1 ~ ARMCD + SEX + ar1(AVISIT | USUBJID), data = fev_data, reml = TRUE)
  conf_coef <- confint(object)
  cis <- matrix(
    c(38.5119, 41.8167, 2.3216, 6.1206, -1.6664, 2.1392),
    ncol = 2, byrow = TRUE
  )
  expect_equal(conf_coef, cis, ignore_attr = TRUE, tolerance = 1e-4)
})
