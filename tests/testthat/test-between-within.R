# h_within_or_between ----

test_that("h_within_or_between works as expected", {
  x_matrix <- cbind(
    "(Intercept)" = 1,
    "AGE" = c(10, 10, 10, 20, 20, 20, 30, 30, 30, 40, 10, 20),
    "VISIT" = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 4, 2),
    "SLOW" = c(1, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )
  subject_ids <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 1, 2))
  result <- expect_silent(h_within_or_between(x_matrix, subject_ids))
  expected <- c("(Intercept)" = "intercept", "AGE" = "between", "VISIT" = "within", "SLOW" = "within")
  expect_identical(result, expected)
})

# h_df_bw_calc ----

test_that("h_df_bw_calc works as expected for the vignette example", {
  object <- get_mmrm()
  result <- expect_silent(h_df_bw_calc(object))

  expect_list(result)
  expect_named(result, c("coefs_between_within", "ddf_between", "ddf_within"))

  expect_identical(result$ddf_between, 192L)
  expect_identical(result$ddf_within, 334L)

  expect_character(result$coefs_between_within)
  expect_snapshot(result$coefs_between_within)
})

test_that("h_df_bw_calc works as expected for a model with only intercept", {
  object <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(h_df_bw_calc(object))

  # Here 197 subjects and 537 observations in total (same as in vignette example) but p1 = p2 = 0.
  # Therefore:
  expect_identical(result$ddf_between, 196L)
  expect_identical(result$ddf_within, 340L)
})

# h_df_min_bw ----

test_that("h_df_min_bw works as expected", {
  object <- get_mmrm()
  bw_calc <- h_df_bw_calc(object)
  coefs <- coef(object)
  is_involved <- setNames(logical(length(coefs)), names(coefs))

  is_involved["AVISITVIS2"] <- TRUE
  result <- expect_silent(h_df_min_bw(bw_calc, is_involved))
  expect_identical(result, bw_calc$ddf_within)

  is_involved["RACEWhite"] <- TRUE
  result <- expect_silent(h_df_min_bw(bw_calc, is_involved))
  expect_identical(result, bw_calc$ddf_between)
})

test_that("h_df_min_bw also works without names (because contrast might not have names)", {
  object <- get_mmrm()
  bw_calc <- h_df_bw_calc(object)
  is_involved <- logical(length(coef(object)))
  is_involved[1L] <- TRUE
  result <- expect_silent(h_df_min_bw(bw_calc, is_involved))
  expect_int(result)
})

# h_df_1d_bw ----

test_that("h_df_1d_bw works as expected for a model with only intercept", {
  object <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(h_df_1d_bw(object, 1))
  expect_list(result)
  expect_equal(result$est, 42.8338, tolerance = 1e-4)
  expect_equal(result$se, 0.3509, tolerance = 1e-4)
  expect_identical(result$df, 340L)
  expect_equal(result$t_stat, 122.07, tolerance = 1e-4)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_1d_bw works as expected for univariate linear combination contrasts", {
  object <- mmrm(
    formula = FEV1 ~ ARMCD + RACE + us(AVISIT | USUBJID),
    data = fev_data
  )

  contrast <- c(0, 0, 1, -1)
  result <- expect_silent(h_df_1d_bw(object, contrast))
  expected_df <- 193L # Because non-zero entries correspond to RACE which is a between-variable.
  expect_identical(result$df, expected_df)

  contrast <- c(1, 0, -1, 0)
  result <- expect_silent(h_df_1d_bw(object, contrast))
  expected_df <- 193L # Because mixed with intercept does not change the minimum.
  expect_identical(result$df, expected_df)
})

test_that("h_df_1d_bw works as expected for singular fits", {
  dat <- fev_data
  dat$ones <- 1
  object <- mmrm(
    formula = FEV1 ~ ones + us(AVISIT | USUBJID),
    data = dat
  )
  object2 <- mmrm(
    formula = FEV1 ~ us(AVISIT | USUBJID),
    data = fev_data
  )
  result <- expect_silent(h_df_1d_bw(object, 1))
  expected <- expect_silent(h_df_1d_bw(object2, 1))
  expect_identical(result, expected)
})


# h_df_md_bw ----

test_that("h_df_md_bw works as expected - between effect", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(result$denom_df, 192L)
  expect_equal(result$f_stat, 36.91, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_bw works as expected - within effect", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 6] <- contrast[2, 7] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(result$denom_df, 334L)
  expect_equal(result$f_stat, 80.96, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_bw works as expected - both effects", {
  object <- get_mmrm()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- contrast[1, 6] <- contrast[2, 7] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  expect_list(result)
  expect_identical(result$num_df, 2L)
  expect_identical(result$denom_df, 192L)
  expect_equal(result$f_stat, 117.65, tolerance = 1e-3)
  expect_true(result$p_val < 0.0001)
})

test_that("h_df_md_bw works as expected for rank deficient model", {
  object <- get_mmrm_rank_deficient()
  contrast <- matrix(data = 0, nrow = 2, ncol = length(component(object, "beta_est")))
  contrast[1, 2] <- contrast[2, 3] <- 1
  result <- expect_silent(h_df_md_bw(object, contrast))
  object2 <- get_mmrm()
  expected <- expect_silent(h_df_md_bw(object2, contrast))
  expect_identical(result, expected)
})
