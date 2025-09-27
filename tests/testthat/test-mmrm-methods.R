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

test_that("h_dataset_sort_all() sorts in column order", {
  expect_equal(
    h_dataset_sort_all(unique(mtcars[c("am", "vs", "cyl")])),
    data.frame(
      am = c(0, 0, 0, 1, 1, 1, 1),
      vs = c(0, 1, 1, 0, 0, 0, 1),
      cyl = c(8, 4, 6, 4, 6, 8, 4),
      row.names =
        c("Hornet Sportabout", "Merc 240D", "Hornet 4 Drive", "Porsche 914-2",
          "Mazda RX4", "Ford Pantera L", "Datsun 710")
    )
  )
})

test_that("h_check_columns_nested() correctly compares dfs", {
  expect_true(h_check_columns_nested(mtcars[, -2:-5], structure(mtcars, a = 1)))
  # FALSE because different numbers of rows:
  expect_false(h_check_columns_nested(mtcars[-2:-5, ], mtcars))
  # FALSE because first dataset has more columns than second dataset:
  expect_false(h_check_columns_nested(transform(mtcars, a = 1), mtcars))
  # FALSE because the first observation is different in each dataset:
  expect_false(h_check_columns_nested(transform(mtcars, vs = c(7, vs[-1])),
                                      mtcars))
})

test_that("h_check_fits_all_data_same() correctly compares dfs", {
  expect_true(h_check_fits_all_data_same(list(get_mmrm_group(),
                                              get_mmrm(),
                                              get_mmrm_rank_deficient())))
  # FALSE because get_mmrm_smaller_data() has a dataset with fewer rows:
  expect_false(h_check_fits_all_data_same(list(get_mmrm_group(),
                                               get_mmrm(),
                                               get_mmrm_smaller_data(),
                                               get_mmrm_rank_deficient())))
  # FALSE because get_mmrm() uses more columns than get_mmrm_group():
  expect_false(h_check_fits_all_data_same(list(get_mmrm(),
                                               get_mmrm_group(),
                                               get_mmrm_rank_deficient())))
  # FALSE because get_mmrm() and get_mmrm_alt_data() have different observations
  expect_false(h_check_fits_all_data_same(list(get_mmrm_group(),
                                               get_mmrm(),
                                               get_mmrm_alt_data(),
                                               get_mmrm_rank_deficient())))
})

test_that("h_fits_common_data() grabs common observations among datasets", {
  expect_equal(
    h_fits_common_data(
      list(get_mmrm(), get_mmrm_alt_data(), get_mmrm_rank_deficient())
    ),
    data.frame(
      FEV1 = c(39.9710497720302, NA),
      AVISIT = factor(2:1, labels = c("VIS1", "VIS2")),
      USUBJID = factor(c(1L, 1L), labels = "PT1"),
      RACE = factor(c(2L, 2L), labels = "Black or African American"),
      SEX = factor(c(2L, 2L), labels = "Female"),
      ARMCD = factor(c(2L, 2L), labels = "TRT"),
      SEX2 = factor(c(2L, 2L), labels = "Female")
    ),
    tolerance = 1e-3
  )
})

test_that("h_get_minimal_fit_data() grabs only colums used in model fitting", {
  expect_equal(
    h_get_minimal_fit_data(get_mmrm_group()),
    fev_data[c("FEV1", "AVISIT", "USUBJID", "ARMCD")]
  )
  expect_equal(
    h_get_minimal_fit_data(get_mmrm()),
    fev_data[c("FEV1", "AVISIT", "USUBJID", "RACE", "SEX", "ARMCD")]
  )

  # Account for model terms that are calls instead of just symbols (e.g., log(x)
  # as opposed to just x)
  expect_equal(
    h_get_minimal_fit_data(get_mmrm_trans()),
    fev_data[c("FEV1", "AVISIT", "USUBJID", "FEV1_BL", "ARMCD")]
  )
})

test_that("h_check_covar_nesting() ensures models have nested covariates", {
  expect_equal(
    h_check_covar_nesting(get_mmrm()[["formula_parts"]],
                          get_mmrm_rank_deficient()[["formula_parts"]]),
    "nested"
  )
  expect_equal(
    h_check_covar_nesting(get_mmrm_group()[["formula_parts"]],
                          get_mmrm_kr()[["formula_parts"]]),
    "identical"
  )
  # First model's covariates aren't nested within the second model's
  expect_error(
    h_check_covar_nesting(get_mmrm_trans()[["formula_parts"]],
                          get_mmrm_tmb_rank_deficient()[["formula_parts"]]),
    regexp = "covariates.+subset"
  )
})

test_that("h_check_cov_struct_nesting() ensures models have nested covariance structures", {
  expect_equal(
    h_check_cov_struct_nesting(get_mmrm_trans()[["formula_parts"]],
                               get_mmrm_tmb()[["formula_parts"]]),
    "nested"
  )
  expect_equal(
    h_check_cov_struct_nesting(get_mmrm_transformed()[["formula_parts"]],
                               get_mmrm_trans()[["formula_parts"]]),
    "identical"
  )
  # Second model is nested within the first rather than the other way around
  expect_error(
    h_check_cov_struct_nesting(get_mmrm_tmb()[["formula_parts"]],
                               get_mmrm_trans()[["formula_parts"]]),
    regexp = "special case of the next model"
  )
})

test_that("h_assert_nested_models() ensures nested models", {
  # Different visit variable
  expect_error(h_assert_nested_models(get_mmrm_group(),
                                      get_mmrm_alt_visit(),
                                      any_reml = TRUE),
               regexp = "visit variable")
  # Different subject
  expect_error(h_assert_nested_models(get_mmrm_group(),
                                      get_mmrm_alt_subj(),
                                      any_reml = TRUE),
               regexp = "subject variable")
  # Different grouping variable
  expect_error(h_assert_nested_models(get_mmrm_group(),
                                      get_mmrm_alt_group(),
                                      any_reml = TRUE),
               regexp = "grouping variable")
  # Nested variables but REML
  expect_error(h_assert_nested_models(get_mmrm(),
                                      get_mmrm_rank_deficient(),
                                      any_reml = TRUE),
               regexp = "REML.+covariates.+same")
  # Identical model warning
  expect_warning(h_assert_nested_models(get_mmrm(),
                                        get_mmrm(),
                                        any_reml = FALSE),
                 regexp = "identical")
  expect_true(h_assert_nested_models(get_mmrm_kr(),
                                     get_mmrm(),
                                     any_reml = FALSE))
})

test_that("h_assert_lrt_suitability() ensures suitability for LRT testing", {
  # non-increasing degrees of freedom
  expect_error(h_assert_lrt_suitability(list(get_mmrm_cs(), get_mmrm_smaller_data()),
                                        refit = FALSE,
                                        dfs = c(3, 3),
                                        is_reml = c(TRUE, TRUE)),
               regexp = "degrees of freedom")
  # refit = FALSE and they don't use the same data
  expect_error(h_assert_lrt_suitability(list(get_mmrm_cs(), get_mmrm_smaller_data()),
                                        refit = FALSE,
                                        dfs = c(attr(logLik(get_mmrm_cs()), "df"),
                                                attr(logLik(get_mmrm_smaller_data()), "df")),
                                        is_reml = c(component(get_mmrm_cs(), "reml"),
                                                    component(get_mmrm_smaller_data(), "reml"))),
               regexp = "same data")
  expect_true(h_assert_lrt_suitability(list(get_mmrm_cs(), get_mmrm()),
                                       refit = FALSE,
                                       dfs = c(attr(logLik(get_mmrm_cs()), "df"), attr(logLik(get_mmrm()), "df")),
                                       is_reml = c(component(get_mmrm_cs(), "reml"), component(get_mmrm(), "reml"))))
})

test_that("h_generate_new_name() generates a string without a binding in env", {
  expect_equal(h_generate_new_name("c", asNamespace("stats")), "c.1")
  this_environment <- environment()
  test_env_parent <- new.env()
  test_env_child <- new.env(parent = test_env_parent)
  this_environment[["foo_mmrm_test_name.2"]] <- NA
  test_env_parent[["foo_mmrm_test_name"]] <- NA
  test_env_child[["foo_mmrm_test_name.1"]] <- NA
  expect_equal(
    h_generate_new_name("foo_mmrm_test_name", test_env_child),
    "foo_mmrm_test_name.3"
  )
})

test_that("h_refit_mmrm() successfully refits an mmrm fit", {
  fit <- get_mmrm()
  refit <- h_refit_mmrm(get_mmrm_smaller_data(), fev_data)

  # Get rid of tmb_object and call's data argument, which will be different
  fit$tmb_object <- NULL
  fit$call$data <- NULL
  refit$tmb_object <- NULL
  refit$call$data <- NULL

  expect_equal(refit, fit)
})

test_that("h_anova_single_mmrm_model() yields the significance of each term", {
  expect_equal(
    h_anova_single_mmrm_model(get_mmrm_group()),
    data.frame(
      num_df = c(1, 1),
      denom_df = c(93.50361, 146.55428),
      f_stat = c(10170.36271, 30.09819),
      p_val = c(3.319442e-97, 1.757614e-7),
      row.names = c("(Intercept)", "ARMCD")
    ),
    tolerance = 1e-3
  )
})

test_that("anova.mmrm() works for a single model", {
  expect_equal(
    anova(get_mmrm_group()),
    structure(
      data.frame(
        num_df = c(1, 1),
        denom_df = c(93.50361, 146.55428),
        f_stat = c(10170.36271, 30.09819),
        p_val = c(3.319442e-97, 1.757614e-7),
        row.names = c("(Intercept)", "ARMCD")
      ),
      class = c("anova.mmrm", "data.frame")
    ),
    tolerance = 1e-3
  )
})



test_that("anova.mmrm() works for multiple models -- no refitting", {
  call_cs <- quote(mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + cs(AVISIT | USUBJID), data = fev_data))
  call_us <- quote(mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID), data = fev_data))
  expect_equal(
    anova(get_mmrm_cs(), get_mmrm()),
    structure(
      data.frame(
        Model = 1L:2L,
        refit = c(FALSE, FALSE),
        REML = c(TRUE, TRUE),
        n_param = c(2, 10),
        n_coef = c(11, 11),
        df = c(2, 10),
        AIC = c(3526.04295, 3406.44988),
        BIC = c(3532.60936, 3439.28191),
        logLik = c(-1761.02147, -1693.22494),
        test = c(NA, "1 vs 2"),
        log_likelihood_ratio = c(NA, 67.79654),
        p_value = c(NA, 1.95508e-25),
        call = c(deparse1(call_cs), deparse1(call_us))
      ),
      class = c("anova.mmrm", "data.frame")
    ),
    tolerance = 1e-3
  )
})




test_that("anova.mmrm() works for multiple models -- with refitting", {
  call_cs <- quote(mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + cs(AVISIT | USUBJID), data = data.1))
  call_us <- quote(mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID), data = .smaller_fev_data))
  expect_equal(
    anova(get_mmrm_cs(), get_mmrm_smaller_data(), refit = TRUE),
    structure(
      data.frame(
        Model = 1L:2L,
        refit = c(TRUE, FALSE),
        REML = c(TRUE, TRUE),
        n_param = c(2, 6),
        n_coef = c(9, 9),
        df = c(2, 6),
        AIC = c(2448.51892, 2418.41620),
        BIC = c(2455.03391, 2437.96118),
        logLik = c(-1222.25946, -1203.20810),
        test = c(NA, "1 vs 2"),
        log_likelihood_ratio = c(NA, 19.05136),
        p_value = c(NA, 1.067197e-7),
        call = c(deparse1(call_cs), deparse1(call_us))
      ),
      class = c("anova.mmrm", "data.frame")
    ),
    tolerance = 1e-3
  )
})
