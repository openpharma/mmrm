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

# h_get_contrast ----
test_that("h_get_contrast works as expected", {
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "log(FEV1_BL)", "3"),
    matrix(c(0, 1, rep(0, 7)), nrow = 1, byrow = TRUE)
  )
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "ARMCD", "3"),
    matrix(c(0, 0, 1, rep(0, 3), rep(0.25, 3)), nrow = 1, byrow = TRUE)
  )
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "ARMCD:AVISIT", "3"),
    matrix(rep(rep(c(0, 1), 3), c(6, 1, 9, 1, 9, 1)), nrow = 3, byrow = TRUE)
  )
})

# Anova ----
test_that("Anova works as expected", {
  expect_snapshot_tolerance(
    Anova(get_mmrm(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_weighted(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_rank_deficient(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_trans(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm(), "2")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_weighted(), "2")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_rank_deficient(), "2")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_trans(), "2")
  )
})

# Anova Integration Test ----

test_that("Anova give similar results to SAS", {
  fit <- mmrm(FEV1 ~ ARMCD * FEV1_BL + ar1(AVISIT | USUBJID), data = fev_data)
  df2 <- expect_silent(Anova(fit, "2"))
  df3 <- expect_silent(Anova(fit, "3"))
  # the SAS results from design/anova/test2_1.csv and design/anova/test3_1.csv
  # they shared the same model
  expect_equal(
    df2$`Denom Df`,
    c(199.582314485653, 196.41571750149, 198.163532037944),
    tolerance = 1e-4
  )
  expect_equal(
    df2$`F Statistic`,
    c(0.03099660681111, 11.5301174147263, 0.7644368094469),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`Denom Df`,
    c(199.582314485653, 198.163532037936, 198.163532037944),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`F Statistic`,
    c(0.03099660681111, 11.0435094615508, 0.7644368094469),
    tolerance = 1e-4
  )
})

test_that("Anova give similar results to SAS", {
  fit <- mmrm(FEV1 ~ ARMCD * SEX + ARMCD * FEV1_BL - FEV1_BL + ar1(AVISIT | USUBJID), data = fev_data)
  df2 <- expect_silent(Anova(fit, "2"))
  df3 <- expect_silent(Anova(fit, "3"))
  # the SAS results from design/anova/test2_1.csv and design/anova/test3_1.csv
  # they shared the same model
  expect_equal(
    df2$`Denom Df`,
    c(196.700026021882, 186.404412481085, 186.11901853766, 194.942672335072),
    tolerance = 1e-4
  )
  expect_equal(
    df2$`F Statistic`,
    c(0.03093826913613, 0.10489707559679, 0.46643535342468, 6.11482167585228),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`Denom Df`,
    c(197.423866403271, 186.119018537661, 186.11901853766, 194.942672335072),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`F Statistic`,
    c(0.04875135180324, 0.09277653315012, 0.46643535342468, 6.11482167585228),
    tolerance = 1e-4
  )
})
