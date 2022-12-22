# mmrm_control ----

test_that("mmrm_control works as expected", {
  result <- mmrm_control(
    optimizer_fun = stats::optim,
    optimizer_args = list(method = "L-BFGS-B")
  )
  expected <- structure(
    list(
      optimizers = result$optimizers,
      start = NULL,
      accept_singular = TRUE,
      method = "Satterthwaite",
      n_cores = 1L,
      drop_visit_levels = TRUE
    ),
    class = "mmrm_control"
  )
  expect_identical(result, expected)
})

# h_mmrm_tmb_exract_terms ---
test_that("h_mmrm_tmb_extract_terms works for covariance terms as expected", {
  expect_identical(
    h_mmrm_tmb_extract_terms(quote(a | b)),
    list(subject_var = "b", visit_var = "a", group_var = NULL)
  )
  expect_error(
    h_mmrm_tmb_extract_terms(quote(a + b | b)),
    "`time` in `time|\\(group/\\)subject` must be specified as one single variable."
  )
  expect_error(
    h_mmrm_tmb_extract_terms(quote(a | (b + c))),
    "Covariance structure must be of the form `time|\\(group/\\)subject`."
  )
  expect_identical(
    h_mmrm_tmb_extract_terms(quote(a | b / c)),
    list(subject_var = "c", visit_var = "a", group_var = "b")
  )
  expect_error(
    h_mmrm_tmb_extract_terms(quote(a | (b + d) / c)),
    "`group` in `time|\\(group/\\)subject` must be specified as one single variable."
  )
  expect_error(
    h_mmrm_tmb_extract_terms(quote(a | b / (c + d))),
    "`subject` in `time|\\(group/\\)subject` must be specified as one single variable."
  )
})

# h_mmrm_tmb_extract_vars ----
test_that("h_mmrm_tmb_extract_vars works for non-grouped formula as expected", {
  expect_identical(
    h_mmrm_tmb_extract_vars(quote(cs(a | b))),
    list(subject_var = "b", visit_var = "a", group_var = NULL, is_spatial = FALSE)
  )
  expect_identical(
    h_mmrm_tmb_extract_vars(quote(sp_exp(a1, a2 | b))),
    list(subject_var = "b", visit_var = c("a1", "a2"), group_var = NULL, is_spatial = TRUE)
  )
  expect_error(
    h_mmrm_tmb_extract_vars(quote(cs(a + b))),
    "Covariance structure must be of the form `time|\\(group/\\)subject`."
  )
})

test_that("h_mmrm_tmb_extract_vars works for grouped formula as expected", {
  expect_identical(
    h_mmrm_tmb_extract_vars(quote(cs(a | b / c))),
    list(subject_var = "c", visit_var = "a", group_var = "b", is_spatial = FALSE)
  )
  expect_error(
    h_mmrm_tmb_extract_vars(quote(cs((a + b) | c / d))),
    "`time` in `time|\\(group/\\)subject` must be specified as one single variable."
  )
  expect_error(
    h_mmrm_tmb_extract_vars(quote(cs(a | b / (c + d)))),
    "`subject` in `time|\\(group/\\)subject` must be specified as one single variable."
  )
  expect_error(
    h_mmrm_tmb_extract_vars(quote(cs(a | (b + c) / d))),
    "`group` in `time|\\(group/\\)subject` must be specified as one single variable."
  )
})


test_that("h_mmrm_tmb_extract_vars works for multiple coordinates as expected", {
  expect_identical(
    h_mmrm_tmb_extract_vars(quote(sp_exp(a1, a2, a3 | b))),
    list(subject_var = "b", visit_var = c("a1", "a2", "a3"), group_var = NULL, is_spatial = TRUE)
  )
  expect_error(
    h_mmrm_tmb_extract_vars(quote(us(a1, a2, a3 | b))),
    "Non-spatial covariance term should not include multiple `time` variables."
  )
  expect_warning(
    h_mmrm_tmb_extract_vars(quote(sp_exp(a1, a1 | b))),
    "Duplicated `time` variable spotted: a1. This may indicate input errors in the formula."
  )
})

# h_mmrm_tmb_formula_parts ----

test_that("h_mmrm_tmb_formula_parts works as expected", {
  result <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  ))
  expected <- structure(
    list(
      formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
      model_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + ARMCD:AVISIT,
      full_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + USUBJID + ARMCD:AVISIT,
      cov_type = "us",
      is_spatial = FALSE,
      visit_var = "AVISIT",
      subject_var = "USUBJID",
      group_var = NULL
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result, expected)

  result_group <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | ARMCD / USUBJID)
  ))
  expected_group <- structure(
    list(
      formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | ARMCD / USUBJID),
      model_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + ARMCD:AVISIT,
      full_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + USUBJID + ARMCD:AVISIT,
      cov_type = "us",
      is_spatial = FALSE,
      visit_var = "AVISIT",
      subject_var = "USUBJID",
      group_var = "ARMCD"
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result_group, expected_group)

  expect_error(
    h_mmrm_tmb_formula_parts(FEV1 ~ RACE + AVISIT + USUBJID),
    paste(
      "Covariance structure must be specified in formula.",
      "Possible covariance structures include: us, toep, toeph, ar1, ar1h, ad, adh, cs, csh"
    )
  )
  expect_error(
    h_mmrm_tmb_formula_parts(FEV1 ~ RACE + arh1(AVISIT | USUBJID)),
    paste(
      "Covariance structure must be specified in formula.",
      "Possible covariance structures include: us, toep, toeph, ar1, ar1h, ad, adh, cs, csh"
    )
  )
  expect_error(
    h_mmrm_tmb_formula_parts(FEV1 ~ RACE + ar1h(AVISIT | USUBJID) + cs(AVISIT | USUBJID)),
    "Only one covariance structure can be specified. Currently specified covariance structures are: ar1h, cs"
  )
  expect_error(
    h_mmrm_tmb_formula_parts(FEV1 ~ RACE + cs(AVISIT)),
    "Covariance structure must be of the form `time\\|\\(group/\\)subject`"
  )
  expect_error(
    h_mmrm_tmb_formula_parts(FEV1 ~ RACE + cs(AVISIT | RACE + ARMCD / USUBJID)),
    "Covariance structure must be of the form `time\\|\\(group/\\)subject`"
  )
  expect_error(
    h_mmrm_tmb_formula_parts(FEV1 ~ RACE + cs(AVISIT, AVISIT | RACE + ARMCD / USUBJID)),
    "Covariance structure must be of the form `time\\|\\(group/\\)subject`"
  )
})

test_that("h_mmrm_tmb_formula_parts works without covariates", {
  result <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ ar1(AVISIT | USUBJID)
  ))
  expected <- structure(
    list(
      formula = FEV1 ~ ar1(AVISIT | USUBJID),
      model_formula = FEV1 ~ 1,
      full_formula = FEV1 ~ USUBJID + AVISIT,
      cov_type = "ar1",
      is_spatial = FALSE,
      visit_var = "AVISIT",
      subject_var = "USUBJID",
      group_var = NULL
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_formula_parts works as expected for antedependence", {
  result <- expect_silent(h_mmrm_tmb_formula_parts(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID)
  ))
  expected <- structure(
    list(
      formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID),
      model_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + ARMCD:AVISIT,
      full_formula = FEV1 ~ RACE + SEX + ARMCD + AVISIT + USUBJID + ARMCD:AVISIT,
      cov_type = "ad",
      is_spatial = FALSE,
      visit_var = "AVISIT",
      subject_var = "USUBJID",
      group_var = NULL
    ),
    class = "mmrm_tmb_formula_parts"
  )
  expect_identical(result, expected)
})

# h_mmrm_tmb_data ----

test_that("h_mmrm_tmb_data works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  result <- expect_silent(h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    fev_data$WEIGHT,
    reml = FALSE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  ))
  expect_class(result, "mmrm_tmb_data")
  expect_named(
    result,
    c(
      "full_frame", "x_matrix", "x_cols_aliased", "coordinates", "y_vector",
      "weights_vector", "visits_zero_inds", "n_visits", "n_subjects",
      "subject_zero_inds", "subject_n_visits", "cov_type", "is_spatial_int", "reml",
      "subject_groups", "n_groups"
    )
  )
  expect_matrix(result$x_matrix, nrows = 537, ncols = 3, any.missing = FALSE)
  expect_numeric(result$y_vector, len = 537, any.missing = FALSE)
  expect_integer(result$visits_zero_inds, len = 537, lower = 0, upper = 3, any.missing = FALSE)
  expect_identical(result$n_visits, 4L) # 4 visits.
  expect_integer(result$subject_zero_inds, len = 197, unique = TRUE, sorted = TRUE, any.missing = FALSE)
  expect_identical(result$cov_type, "us") # unstructured.
  expect_identical(result$reml, 0L) # ML.
  expect_identical(result$subject_groups, factor(rep(0L, 197))) # all in the same group
  expect_identical(result$n_groups, 1L) # number of groups
})

test_that("h_mmrm_tmb_data works as expected for grouped covariance", {
  formula <- FEV1 ~ RACE + us(AVISIT | ARMCD / USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  result <- expect_silent(h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    reml = FALSE,
    weights = rep(1, nrow(fev_data)),
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  ))
  expect_class(result, "mmrm_tmb_data")
  expect_named(
    result,
    c(
      "full_frame", "x_matrix", "x_cols_aliased", "coordinates", "y_vector",
      "weights_vector", "visits_zero_inds", "n_visits", "n_subjects", "subject_zero_inds",
      "subject_n_visits", "cov_type", "is_spatial_int", "reml", "subject_groups", "n_groups"
    )
  )
  expect_matrix(result$x_matrix, nrows = 537, ncols = 3, any.missing = FALSE)
  expect_numeric(result$y_vector, len = 537, any.missing = FALSE)
  expect_integer(result$visits_zero_inds, len = 537, lower = 0, upper = 3, any.missing = FALSE)
  expect_identical(result$n_visits, 4L) # 4 visits.
  expect_integer(result$subject_zero_inds, len = 197, unique = TRUE, sorted = TRUE, any.missing = FALSE)
  expect_identical(result$cov_type, "us") # unstructured.
  expect_identical(result$reml, 0L) # ML.
  expect_factor(result$subject_groups, levels = c("PBO", "TRT")) # ARMCD is the group
  expect_identical(result$n_groups, 2L) # number of groups
})

test_that("h_mmrm_tmb_data works as expected for mutli-dimensional spatial exponential covariance", {
  formula <- FEV1 ~ RACE + sp_exp(VISITN, VISITN2 | ARMCD / USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  result <- expect_silent(h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    reml = FALSE,
    weights = rep(1, nrow(fev_data)),
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  ))
  expect_class(result, "mmrm_tmb_data")
  expect_named(
    result,
    c(
      "full_frame", "x_matrix", "x_cols_aliased", "coordinates", "y_vector",
      "weights_vector", "visits_zero_inds", "n_visits", "n_subjects", "subject_zero_inds",
      "subject_n_visits", "cov_type", "is_spatial_int", "reml", "subject_groups", "n_groups"
    )
  )
  expect_matrix(result$x_matrix, nrows = 537, ncols = 3, any.missing = FALSE)
  expect_numeric(result$y_vector, len = 537, any.missing = FALSE)
  expect_identical(result$n_visits, 4L)
  expect_integer(result$subject_zero_inds, len = 197, unique = TRUE, sorted = TRUE, any.missing = FALSE)
  expect_identical(result$cov_type, "sp_exp") # spatial exponential
  expect_identical(result$reml, 0L) # ML.
  expect_factor(result$subject_groups, levels = c("PBO", "TRT")) # ARMCD is the group
  expect_identical(result$n_groups, 2L) # number of groups
  expect_matrix(result$coordinates, nrows = 537L, ncols = 2L)
})

test_that("h_mmrm_tmb_data works also for character ID variable", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  dat <- fev_data
  dat$USUBJID <- as.character(dat$USUBJID) # nolint
  result <- expect_silent(h_mmrm_tmb_data(
    formula_parts,
    dat,
    weights = rep(1, nrow(dat)),
    reml = FALSE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  ))
  expected <- expect_silent(h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = FALSE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  ))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_data correctly processes design matrix below full rank correctly", {
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  dat <- fev_data[11:25, ]
  result <- expect_silent(h_mmrm_tmb_data(
    formula_parts,
    dat,
    weights = rep(1, nrow(dat)),
    reml = FALSE,
    accept_singular = TRUE,
    drop_visit_levels = TRUE
  ))
  assert_true(qr(result$x_matrix)$rank == ncol(result$x_matrix))
  assert_true(sum(result$x_cols_aliased) == 2)
  assert_set_equal(names(which(!result$x_cols_aliased)), colnames(result$x_matrix))
})

test_that("h_mmrm_tmb_data gives error for rank deficient design matrix when not accepted", {
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  dat <- fev_data[11:25, ]
  expect_error(
    h_mmrm_tmb_data(
      formula_parts,
      dat,
      weights = rep(1, nrow(dat)),
      reml = FALSE,
      accept_singular = FALSE,
      drop_visit_levels = TRUE
    ),
    paste(
      "design matrix only has rank 8 and 2 columns (ARMCDTRT:AVISITVIS2, ARMCDTRT:AVISITVIS3)",
      "could be dropped to achieve full rank 10 by using `accept_singular = TRUE`"
    ),
    fixed = TRUE
  )
})

test_that("h_mmrm_tmb_data catches case with multiple time points per subject early", {
  formula_parts <- structure(
    list(
      formula = y ~ cd + ad(visit | id),
      model_formula = y ~ cd,
      full_formula = y ~ cd + id + visit,
      cov_type = "ad",
      is_spatial = FALSE,
      visit_var = "visit",
      subject_var = "id",
      group_var = NULL
    ),
    class = "mmrm_tmb_formula_parts"
  )

  set.seed(123)
  dat <- data.frame(
    id = as.factor(c(1, 1, 1, 2, 2, 2)),
    visit = as.factor(c(1, 2, 3, 2, 2, 3)),
    cd = c(1, 2, 3, 4, 5, 6),
    y = rnorm(6)
  )

  expect_error(
    h_mmrm_tmb_data(
      formula_parts,
      data = dat,
      weights = rep_len(1, nrow(dat)),
      reml = TRUE,
      accept_singular = TRUE,
      drop_visit_levels = TRUE
    ),
    paste0(
      "time points have to be unique for each subject, detected following duplicates in data:",
      "\\s+id visit\\s+5\\s+2\\s+2" # Make sure we get the nice duplicates printed there.
    )
  )
})


test_that("h_mmrm_tmb_data has no side effect of overwrite the weights in global env", {
  weights <- "this is global weights"
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  res <- h_mmrm_tmb_data(
    formula_parts,
    data = fev_data,
    weights = rep_len(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = TRUE,
    drop_visit_levels = TRUE
  )
  expect_identical(weights, "this is global weights")
})

test_that("h_mmrm_tmb_data will not be affecte by `weights` in data", {
  weights <- fev_data$WEIGHT
  fev_data$weights <- weights
  formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  res <- h_mmrm_tmb_data(
    formula_parts,
    data = fev_data,
    weights = rep_len(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = TRUE,
    drop_visit_levels = TRUE
  )
  expect_identical(res$weights_vector, rep_len(1, nrow(res$x_matrix)))
  expect_identical(fev_data$weights, weights)
})

# h_mmrm_tmb_parameters ----

test_that("h_mmrm_tmb_parameters works as expected without start values", {
  formula <- FEV1 ~ SEX + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 10))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with start values", {
  formula <- FEV1 ~ SEX + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  start <- 1:10
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = start))
  expected <- list(theta = start)
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with antedependence", {
  formula <- FEV1 ~ SEX + ad(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 4)) # 4 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with heterogeneous antedependence", {
  formula <- FEV1 ~ SEX + adh(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 7)) # 2 * 4 - 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with Toeplitz", {
  formula <- FEV1 ~ SEX + toep(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 4)) # 4 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with heterogeneous Toeplitz", {
  formula <- FEV1 ~ SEX + toeph(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 7)) # 2 * 4 - 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with autoregressive", {
  formula <- FEV1 ~ SEX + ar1(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = c(0, 0.5))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with heterogeneous autoregressive", {
  formula <- FEV1 ~ SEX + ar1h(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = c(rep(0, 4), 0.5)) # 4 + 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with compound symmetry", {
  formula <- FEV1 ~ SEX + cs(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 2))
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with heterogeneous compound symmetry", {
  formula <- FEV1 ~ SEX + csh(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = TRUE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 5)) # 4 + 1 parameters.
  expect_identical(result, expected)
})

test_that("h_mmrm_tmb_parameters works as expected with spatial exponential", {
  formula <- FEV1 ~ SEX + sp_exp(VISITN | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    reml = TRUE,
    accept_singular = FALSE,
    weights = rep(1, nrow(fev_data)),
    drop_visit_levels = TRUE
  )
  result <- expect_silent(h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL))
  expected <- list(theta = rep(0, 2)) # 1 + 1 parameters.
  expect_identical(result, expected)
})

# h_mmrm_tmb_assert_start ----

test_that("h_mmrm_tmb_assert_start passes as expected for sane start values", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) par,
    par = 1:2
  )
  result <- expect_silent(h_mmrm_tmb_assert_start(tmb_object))
  expect_null(result)
})

test_that("h_mmrm_tmb_assert_start fails as expected for NaN objective function at start", {
  tmb_object <- list(
    fn = function(par) NaN,
    gr = function(par) par,
    par = 1:2
  )
  expect_error(
    h_mmrm_tmb_assert_start(tmb_object),
    "negative log-likelihood is NaN at starting parameter values"
  )
})

test_that("h_mmrm_tmb_assert_start fails as expected for NaN gradient function at start", {
  tmb_object <- list(
    fn = function(par) sum(par),
    gr = function(par) c(NaN, 0.5),
    par = 1:2
  )
  expect_error(
    h_mmrm_tmb_assert_start(tmb_object),
    "some elements of gradient are NaN at starting parameter values"
  )
})

# h_mmrm_tmb_check_conv ----

test_that("h_mmrm_tmb_check_conv passes as expected for sane optimization result", {
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 0,
    message = NULL
  )
  mmrm_tmb <- structure(
    list(theta_vcov = diag(1)),
    class = "mmrm_tmb"
  )
  result <- expect_silent(h_mmrm_tmb_check_conv(tmb_opt, mmrm_tmb))
  expect_null(result)
})

test_that("h_mmrm_tmb_check_conv raises singular hessian warning as expected", {
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 0,
    message = NULL
  )
  mmrm_tmb <- structure(
    list(theta_vcov = try(solve(matrix(0, 1, 1)), silent = TRUE)),
    class = "mmrm_tmb"
  )
  expect_warning(
    h_mmrm_tmb_check_conv(tmb_opt, mmrm_tmb),
    "Model convergence problem: hessian is singular, theta_vcov not available"
  )
})

test_that("h_mmrm_tmb_check_conv warns if convergence code signals non-convergence", {
  tmb_opt <- list(
    par = 1:5,
    objective = 10,
    convergence = 1,
    message = "something ugly"
  )
  mmrm_tmb <- structure(
    list(theta_vcov = diag(1)),
    class = "mmrm_tmb"
  )
  expect_warning(
    h_mmrm_tmb_check_conv(tmb_opt, mmrm_tmb),
    "Model convergence problem: something ugly."
  )
})

# h_mmrm_tmb_extract_cov ----

test_that("h_mmrm_tmb_extract_cov works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    reml = FALSE,
    weights = rep(1, nrow(fev_data)),
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL)
  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_opt <- with(
    tmb_object,
    do.call(
      what = stats::nlminb,
      args = list(par, fn, gr, hessian = he)
    )
  )
  tmb_report <- tmb_object$report(par = tmb_opt$par)
  result <- h_mmrm_tmb_extract_cov(tmb_report, tmb_data, "AVISIT", FALSE)
  expect_identical(
    colnames(result),
    sprintf("VIS%d", 1:4)
  )
  expect_identical(
    rownames(result),
    sprintf("VIS%d", 1:4)
  )
})

test_that("h_mmrm_tmb_extract_cov works as expected for group covariance", {
  formula <- FEV1 ~ RACE + ar1(AVISIT | ARMCD / USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    reml = FALSE,
    weights = rep(1, nrow(fev_data)),
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL, n_groups = 2L)
  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_opt <- with(
    tmb_object,
    do.call(
      what = stats::nlminb,
      args = list(par, fn, gr, hessian = he)
    )
  )
  tmb_report <- tmb_object$report(par = tmb_opt$par)
  result <- h_mmrm_tmb_extract_cov(tmb_report, tmb_data, formula_parts$visit_var, formula_parts$is_spatial)
  expect_identical(
    names(result),
    c("PBO", "TRT")
  )
  expect_identical(
    colnames(result$PBO),
    sprintf("VIS%d", 1:4)
  )
  expect_identical(
    rownames(result$PBO),
    sprintf("VIS%d", 1:4)
  )
})

# h_mmrm_tmb_fit ----

test_that("h_mmrm_tmb_fit works as expected", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  weights <- rep(1, nrow(fev_data))
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = weights,
    reml = FALSE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL)
  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_opt <- with(
    tmb_object,
    do.call(
      what = stats::nlminb,
      args = list(par, fn, gr, hessian = he)
    )
  )
  result <- expect_silent(h_mmrm_tmb_fit(
    tmb_object,
    tmb_opt,
    fev_data,
    weights,
    formula_parts,
    tmb_data
  ))
  expect_class(result, "mmrm_tmb")
  expect_named(result, c(
    "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
    "neg_log_lik", "formula_parts", "data", "weights",
    "reml", "opt_details", "tmb_object", "tmb_data"
  ))
  expect_identical(rownames(result$cov), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_identical(colnames(result$cov), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_named(result$beta_est, colnames(tmb_data$x_matrix))
  expect_identical(rownames(result$beta_vcov), colnames(tmb_data$x_matrix))
  expect_identical(colnames(result$beta_vcov), colnames(tmb_data$x_matrix))
  expect_matrix(result$theta_vcov, nrows = length(result$theta_est), ncols = length(result$theta_est))
  expect_number(result$neg_log_lik)
  expect_list(result$formula_parts)
  expect_data_frame(result$data)
  expect_false(result$reml)
  expect_list(result$opt_details)
  expect_list(result$tmb_object)
  expect_class(result$tmb_data, "mmrm_tmb_data")
})

test_that("h_mmrm_tmb_fit works as expected for grouped covariance", {
  formula <- FEV1 ~ RACE + cs(AVISIT | ARMCD / USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  weights <- rep(1, nrow(fev_data))
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    reml = TRUE,
    weights = weights,
    accept_singular = TRUE,
    drop_visit_levels = TRUE
  )
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL, n_group = tmb_data$n_groups)
  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_opt <- with(
    tmb_object,
    do.call(
      what = stats::nlminb,
      args = list(par, fn, gr, hessian = he)
    )
  )
  result <- expect_silent(h_mmrm_tmb_fit(
    tmb_object, tmb_opt, fev_data, weights, formula_parts, tmb_data
  ))
  expect_class(result, "mmrm_tmb")
  expect_named(result, c(
    "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
    "neg_log_lik", "formula_parts", "data", "weights", "reml", "opt_details", "tmb_object",
    "tmb_data"
  ))
  expect_identical(rownames(result$cov$PBO), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_identical(colnames(result$cov$PBO), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_identical(rownames(result$cov$TRT), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_identical(colnames(result$cov$TRT), c("VIS1", "VIS2", "VIS3", "VIS4"))
  expect_named(result$beta_est, colnames(tmb_data$x_matrix))
  expect_identical(rownames(result$beta_vcov), colnames(tmb_data$x_matrix))
  expect_identical(colnames(result$beta_vcov), colnames(tmb_data$x_matrix))
  expect_matrix(result$theta_vcov, nrows = length(result$theta_est), ncols = length(result$theta_est))
  expect_number(result$neg_log_lik)
  expect_list(result$formula_parts)
  expect_data_frame(result$data)
  expect_true(result$reml)
  expect_list(result$opt_details)
  expect_list(result$tmb_object)
  expect_class(result$tmb_data, "mmrm_tmb_data")
})

test_that("h_mmrm_tmb_fit errors when an invalid covariance type is used", {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  formula_parts <- h_mmrm_tmb_formula_parts(formula)

  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    fev_data,
    weights = rep(1, nrow(fev_data)),
    reml = FALSE,
    accept_singular = FALSE,
    drop_visit_levels = TRUE
  )
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = NULL)

  tmb_data$cov_type <- "gaaah"
  expect_error(
    TMB::MakeADFun(
      data = tmb_data,
      parameters = tmb_parameters,
      hessian = TRUE,
      DLL = "mmrm",
      silent = TRUE
    ),
    "Unknown covariance type 'gaaah'"
  )
  tmb_data$is_spatial <- TRUE
  expect_error(
    TMB::MakeADFun(
      data = tmb_data,
      parameters = tmb_parameters,
      hessian = TRUE,
      DLL = "mmrm",
      silent = TRUE
    ),
    "Unknown covariance type 'gaaah'"
  )
})

# h_mmrm_tmb ----

## unstructured ----

test_that("h_mmrm_tmb works as expected in a simple model without covariates and ML", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  expect_list(result)
  expect_named(
    result,
    c(
      "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
      "neg_log_lik", "formula_parts", "data", "weights", "reml", "opt_details", "tmb_object",
      "tmb_data", "call"
    )
  )
  # See design/SAS/sas_log_simple.txt for the source of numbers.
  expect_equal(2 * result$neg_log_lik, 3700.68707570)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3499, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.8367, tolerance = 1e-4)
  result_cov_tri <- result$cov[lower.tri(result$cov)]
  expected_cov_tri <- c(49.8121, 2.6246, -40.5833, 4.8398, -8.6607, 22.9163)
  expect_equal(result_cov_tri, expected_cov_tri, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works as expected in a simple model without covariates and REML", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  expect_list(result)
  expect_named(
    result,
    c(
      "cov", "beta_est", "beta_vcov", "theta_est", "theta_vcov",
      "neg_log_lik", "formula_parts", "data", "weights", "reml", "opt_details", "tmb_object",
      "tmb_data", "call"
    )
  )
  # See design/SAS/sas_log_simple_reml.txt for the source of numbers.
  expect_equal(2 * result$neg_log_lik, 3700.94648570)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3509, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.8338, tolerance = 1e-4)
  result_cov_tri <- result$cov[lower.tri(result$cov)]
  expected_cov_tri <- c(49.8999, 2.7459, -40.4566, 4.9722, -8.5335, 23.0555)
  expect_equal(result_cov_tri, expected_cov_tri, tolerance = 1e-3)
})

## ante-dependence ----

### homogeneous ----

test_that("h_mmrm_tmb works with ad covariance structure and ML", {
  formula <- FEV1 ~ ad(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/mmrm_ad_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3855.8794)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.5222, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 43.0574, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(2.2695640, 0.9147445, 0.7249571, 0.1897019)
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with ad covariance structure and REML", {
  formula <- FEV1 ~ ad(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/mmrm_ad_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3855.3383)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.5236, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 43.0560, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(2.2713398, 0.9175433, 0.7303929, 0.1919114)
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

### heterogeneous ----

test_that("h_mmrm_tmb works with adh covariance structure and ML", {
  formula <- FEV1 ~ adh(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_adh_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3713.24501787)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3519, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.9019, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(114.78, 44.5191, 26.7673, 158.33))),
    map_to_theta(c(0.7104, 0.09992, 0.3650))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with adh covariance structure and REML", {
  formula <- FEV1 ~ adh(AVISIT | USUBJID)
  data <- fev_data
  result <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_adh_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3713.49317786)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3529, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.9009, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(114.89, 44.6313, 26.8922, 158.47))),
    map_to_theta(c(0.7106, 0.1033, 0.3657))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

### grouped heterogeneous----

test_that("h_mmrm_tmb works with grouped adh covariance structure and ML", {
  formula <- FEV1 ~ adh(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_adh_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3688.48731427)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3339, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.5284, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(134.26, 51.4797, 19.8318, 114.89))),
    map_to_theta(c(0.8038, 0.02288, 0.1635)),
    log(sqrt(c(83.6194, 32.8289, 38.6672, 215.45))),
    map_to_theta(c(0.5169, 0.1976, 0.5654))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with grouped adh covariance structure and REML", {
  formula <- FEV1 ~ adh(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_adh_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3688.84024388)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3349, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.5293, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(134.41, 51.5930, 19.9393, 114.99))),
    map_to_theta(c(0.8039, 0.02646, 0.1643)),
    log(sqrt(c(83.7418, 32.9411, 38.7701, 215.53))),
    map_to_theta(c(0.5178, 0.2002, 0.5655))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

## toeplitz ----

### homogeneous ----

test_that("h_mmrm_tmb works with toep covariance structure and ML", {
  formula <- FEV1 ~ toep(AVISIT | USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_toep_ml.rtf for the source of numbers.
  expect_equal(deviance(result), 3857.00777313)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4669, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 42.3717, tolerance = 1e-4)
  expected_var <- c(88.8193)
  cor_mat <- VarCorr(result)
  result_var <- as.numeric(diag(cor_mat)[1])
  expect_equal(result_var, expected_var, tolerance = 1e-4)
  result_low_tri <- cor_mat[lower.tri(cor_mat)]
  expected_low_tri <- c(41.2525, 6.7009, -17.7604, 41.2525, 6.7009, 41.2525)
  expect_equal(result_low_tri, expected_low_tri, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with toep covariance structure and REML", {
  formula <- FEV1 ~ toep(AVISIT | USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_toep_reml.rtf for the source of numbers.
  expect_equal(deviance(result), 3856.68995273)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4684, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.3721, tolerance = 1e-4)
  expected_var <- c(89.0847)
  cor_mat <- VarCorr(result)
  result_var <- as.numeric(diag(cor_mat)[1])
  expect_equal(result_var, expected_var, tolerance = 1e-4)
  result_low_tri <- cor_mat[lower.tri(cor_mat)]
  expected_low_tri <- c(41.5499, 6.9536, -17.6135, 41.5499, 6.9536, 41.5499)
  expect_equal(result_low_tri, expected_low_tri, tolerance = 1e-4)
})

### heterogeneous ----

test_that("h_mmrm_tmb works with toeph covariance structure and ML", {
  formula <- FEV1 ~ toeph(AVISIT | USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_toeph_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3722.29178558)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3812, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.6769, tolerance = 1e-4)
  expected_var <- c(90.3084, 38.4717, 35.9728, 175.50)
  cor_mat <- VarCorr(result)
  result_var <- as.numeric(diag(cor_mat))
  expect_equal(result_var, expected_var, tolerance = 1e-4)
  result_low_tri <- cor_mat[lower.tri(cor_mat)]
  expected_low_tri <- c(25.2231, 2.4950, -39.4085, 15.9192, 3.5969, 34.0009)
  expect_equal(result_low_tri, expected_low_tri, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with toeph covariance structure and REML", {
  formula <- FEV1 ~ toeph(AVISIT | USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_toeph_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3722.38018329)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3822, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.6726, tolerance = 1e-4)
  expected_var <- c(90.4041, 38.5927, 36.1518, 175.78)
  cor_mat <- VarCorr(result)
  result_var <- as.numeric(diag(cor_mat))
  expect_equal(result_var, expected_var, tolerance = 1e-4)
  result_low_tri <- cor_mat[lower.tri(cor_mat)]
  expected_low_tri <- c(25.3713, 2.6128, -39.2746, 16.0440, 3.7643, 34.2409)
  expect_equal(result_low_tri, expected_low_tri, tolerance = 1e-4)
})

### grouped heterogeneous ----

test_that("h_mmrm_tmb works with grouped toeph covariance structure and ML", {
  formula <- FEV1 ~ toeph(AVISIT | ARMCD / USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(fit_mmrm(formula, data, weights = rep(1, nrow(fev_data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_toeph_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3704.27043196)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3553, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.4792, tolerance = 1e-4)
  expected_var <- c(104.38, 40.7267, 24.9011, 127.01)
  cor_mat <- VarCorr(result)
  # PBO covariance matrix.
  expected_pbo_var <- c(104.38, 40.7267, 24.9011, 127.01)
  result_pbo_var <- as.numeric(diag(cor_mat$PBO))
  expect_equal(result_pbo_var, expected_pbo_var, tolerance = 1e-3)
  expected_pbo_rho <- c(0.3296, -0.1196, -0.3575)
  result_pbo_rho <- map_to_cor(result$theta_est[5:7])
  expect_equal(result_pbo_rho, expected_pbo_rho, tolerance = 1e-2)
  # TRT covariance matrix.
  expected_trt_var <- c(74.4963, 34.1465, 49.7033, 220.01)
  result_trt_var <- as.numeric(diag(cor_mat$TRT))
  expect_equal(result_trt_var, expected_trt_var, tolerance = 1e-4)
  expected_trt_rho <- c(0.4663, 0.1525, -0.2756)
  result_trt_rho <- map_to_cor(result$theta_est[12:14])
  expect_equal(result_trt_rho, expected_trt_rho, tolerance = 1e-2)
})

test_that("h_mmrm_tmb works with grouped toeph covariance structure and REML", {
  formula <- FEV1 ~ toeph(AVISIT | ARMCD / USUBJID)
  data <- fev_data
  # We have seen transient NA/NaN function evaluation warnings here.
  result <- suppressWarnings(fit_mmrm(formula, data, weights = rep(1, nrow(fev_data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_toeph_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3704.49921127)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3563, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 41.4766, tolerance = 1e-4)
  cor_mat <- VarCorr(result)
  # PBO covariance matrix.
  expected_pbo_var <- c(104.43, 40.8152, 25.0541, 127.27)
  result_pbo_var <- as.numeric(diag(cor_mat$PBO))
  expect_equal(result_pbo_var, expected_pbo_var, tolerance = 1e-3)
  expected_pbo_rho <- c(0.3315, -0.1172, -0.3567)
  result_pbo_rho <- map_to_cor(result$theta_est[5:7])
  expect_equal(result_pbo_rho, expected_pbo_rho, tolerance = 1e-2)
  # TRT covariance matrix.
  expected_trt_var <- c(74.6153, 34.2766, 49.8505, 220.20)
  result_trt_var <- as.numeric(diag(cor_mat$TRT))
  expect_equal(result_trt_var, expected_trt_var, tolerance = 1e-3)
  expected_trt_rho <- c(0.4677, 0.1540, -0.2741)
  result_trt_rho <- map_to_cor(result$theta_est[12:14])
  expect_equal(result_trt_rho, expected_trt_rho, tolerance = 1e-2)
})

## autoregressive ----

### homogeneous ----

test_that("h_mmrm_tmb works with ar1 covariance structure and ML", {
  formula <- FEV1 ~ ar1(AVISIT | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3875.95352406)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.5000, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.3252, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(88.7088)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.4249
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with ar1 covariance structure and REML", {
  formula <- FEV1 ~ ar1(AVISIT | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3875.49945459)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.5013, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.3255, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(88.9866)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.4272
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

### heterogeneous ----

test_that("h_mmrm_tmb works with ar1h covariance structure and ML", {
  formula <- FEV1 ~ ar1h(AVISIT | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1h_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3739.94416707)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3936, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.7017, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(92.2283, 38.3927, 35.0194, 176.08))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.4113
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with ar1h covariance structure and REML", {
  formula <- FEV1 ~ ar1h(AVISIT | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_ar1h_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3739.96835012)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3947, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.6955, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(92.3009, 38.5178, 35.2148, 176.37))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.4130
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

### grouped homogeneous----
test_that("h_mmrm_tmb works with grouped ar1 covariance structure and ML", {
  formula <- FEV1 ~ ar1(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_ar1h_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3873.08507919)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4947, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.9560, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[c(1, 3)])
  expected_sd <- sqrt(c(78.3954, 100.51))
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(2, 4)])
  expected_rho <- c(0.3667, 0.4813)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with grouped ar1 covariance structure and REML", {
  formula <- FEV1 ~ ar1(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_ar1h_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3872.65212395)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4960, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.9576, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[c(1, 3)])
  expected_sd <- sqrt(c(78.6615, 100.79))
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(2, 4)])
  expected_rho <- c(0.3693, 0.4831)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

### grouped heterogeneous----
test_that("h_mmrm_tmb works with grouped ar1h covariance structure and ML", {
  formula <- FEV1 ~ ar1h(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_ar1h_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3724.22021102)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3713, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.7161, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[c(1:4, 6:9)])
  expected_sds <- sqrt(c(109.37, 42.7363, 23.3590, 128.21, 76.5687, 34.2447, 46.8888, 219.43))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(5, 10)])
  expected_rho <- c(0.3160, 0.4583)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with grouped ar1h covariance structure and REML", {
  formula <- FEV1 ~ ar1h(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_ar1h_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3724.36099760)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.37233778, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.7104, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[c(1:4, 6:9)])
  expected_sds <- sqrt(
    c(
      109.37350823, 42.81343590, 23.52559796, 128.45926562,
      76.67358836, 34.38779235, 47.06871404, 219.70601878
    )
  )
  expect_equal(result_sds, expected_sds, tolerance = 1e-3)
  result_rho <- map_to_cor(result$theta_est[c(5, 10)])
  expected_rho <- c(0.3179, 0.4599)
  expect_equal(result_rho, expected_rho, tolerance = 1e-4)
})

## compound symmetry ----

### homogeneous ----

test_that("h_mmrm_tmb works with cs covariance structure and ML", {
  formula <- FEV1 ~ cs(AVISIT | USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_cs_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3918.12214544)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4273, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 42.2897, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(86.7143)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.06596
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

test_that("h_mmrm_tmb works with cs covariance structure and REML", {
  formula <- FEV1 ~ cs(AVISIT | USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_cs_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3917.98183508)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4285, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.2896, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[1])
  expected_sd <- sqrt(86.8968)
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[2])
  expected_rho <- 0.06783
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

### heterogeneous ----

test_that("h_mmrm_tmb works with csh covariance structure and ML", {
  formula <- FEV1 ~ csh(AVISIT | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_csh_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3784.63158043)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3566, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.0018, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(104.54, 38.5253, 31.1656, 178.62))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.1566
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with csh covariance structure and REML", {
  formula <- FEV1 ~ csh(AVISIT | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_csh_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3784.85341599)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3575, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.9964, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[1:4])
  expected_sds <- sqrt(c(104.61, 38.6137, 31.3143, 178.91))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[5])
  expected_rho <- 0.1582
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

## weighted mmrm ----

test_that("h_mmrm_tmb works with weights and ML", {
  formula <- FEV1 ~ adh(AVISIT | USUBJID)
  data <- fev_data
  weights <- fev_data$WEIGHT
  result <- expect_silent(fit_mmrm(formula, data, weights, reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_weighted_mmrm_ml.rtf for the source of numbers.
  expect_equal(deviance(result), 3800.20590355)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3477, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 42.3381, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(59.9705, 19.3098, 16.0271, 83.0741))),
    map_to_theta(c(0.6200, 0.08924, 0.3638))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

test_that("h_mmrm_tmb works with weights and REML", {
  formula <- FEV1 ~ adh(AVISIT | USUBJID)
  data <- fev_data
  weights <- fev_data$WEIGHT
  result <- expect_silent(fit_mmrm(formula, data, weights, reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_weighted_mmrm_reml.rtf for the source of numbers.
  expect_equal(deviance(result), 3800.47793975)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3487, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 42.3354, tolerance = 1e-4)
  result_theta <- result$theta_est
  expected_theta <- c(
    log(sqrt(c(60.0152, 19.3624, 16.0953, 83.1570))),
    map_to_theta(c(0.6202, 0.09264, 0.3646))
  )
  expect_equal(result_theta, expected_theta, tolerance = 1e-4)
})

### grouped homogeneous ----

test_that("h_mmrm_tmb works with group cs covariance structure and ML", {
  formula <- FEV1 ~ cs(AVISIT | ARMCD / USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_cs_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3915.54243738)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4236, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 41.9714, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[c(1, 3)])
  expected_sd <- sqrt(c(77.9218, 96.1798))
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(2, 4)])
  expected_rho <- c(3.0393, 8.8558) / c(77.9218, 96.1798)
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

test_that("h_mmrm_tmb works with cs covariance structure and REML", {
  formula <- FEV1 ~ cs(AVISIT | ARMCD / USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_cs_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3915.41985780)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4247, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 41.9734, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[c(1, 3)])
  expected_sd <- sqrt(c(78.1081, 96.3502))
  expect_equal(result_sd, expected_sd, tolerance = 1e-3)
  result_rho <- map_to_cor(result$theta_est[c(2, 4)])
  expected_rho <- c(3.2130, 9.0248) / c(78.1081, 96.3502)
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

### grouped heterogeneous----

test_that("h_mmrm_tmb works with grouped csh covariance structure and ML", {
  formula <- FEV1 ~ csh(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_csh_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3764.21336404)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3363, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.8492, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[c(1:4, 6:9)])
  expected_sds <- sqrt(c(122.6, 45.101, 21.2903, 125.92, 82.6, 31.2298, 44.5561, 234.25))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(5, 10)])
  expected_rho <- c(0.07915, 0.2082)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with grouped csh covariance structure and REML", {
  formula <- FEV1 ~ csh(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_csh_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3764.55218946)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3372, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.8458, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[c(1:4, 6:9)])
  expected_sds <- sqrt(c(122.64, 45.1705, 21.411, 126.14, 82.7175, 31.3308, 44.6841, 234.52))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(5, 10)])
  expected_rho <- c(0.08074, 0.2097)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})


### grouped homogeneous ----

test_that("h_mmrm_tmb works with group cs covariance structure and ML", {
  formula <- FEV1 ~ cs(AVISIT | ARMCD / USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_cs_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3915.54243738)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4236, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 41.9714, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[c(1, 3)])
  expected_sd <- sqrt(c(77.9218, 96.1798))
  expect_equal(result_sd, expected_sd, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(2, 4)])
  expected_rho <- c(3.0393, 8.8558) / c(77.9218, 96.1798)
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

test_that("h_mmrm_tmb works with cs covariance structure and REML", {
  formula <- FEV1 ~ cs(AVISIT | ARMCD / USUBJID)
  # We can get transient warnings here.
  result <- suppressWarnings(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE))
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_cs_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3915.41985780)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.4247, tolerance = 1e-3)
  expect_equal(as.numeric(result$beta_est), 41.9734, tolerance = 1e-4)
  result_sd <- exp(result$theta_est[c(1, 3)])
  expected_sd <- sqrt(c(78.1081, 96.3502))
  expect_equal(result_sd, expected_sd, tolerance = 1e-3)
  result_rho <- map_to_cor(result$theta_est[c(2, 4)])
  expected_rho <- c(3.2130, 9.0248) / c(78.1081, 96.3502)
  expect_equal(result_rho, expected_rho, tolerance = 1e-2)
})

### grouped heterogeneous----

test_that("h_mmrm_tmb works with grouped csh covariance structure and ML", {
  formula <- FEV1 ~ csh(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_csh_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3764.21336404)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3363, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.8492, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[c(1:4, 6:9)])
  expected_sds <- sqrt(c(122.6, 45.101, 21.2903, 125.92, 82.6, 31.2298, 44.5561, 234.25))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(5, 10)])
  expected_rho <- c(0.07915, 0.2082)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with grouped csh covariance structure and REML", {
  formula <- FEV1 ~ csh(AVISIT | ARMCD / USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_group_csh_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3764.55218946)
  expect_equal(sqrt(result$beta_vcov[1, 1]), 0.3372, tolerance = 1e-4)
  expect_equal(as.numeric(result$beta_est), 41.8458, tolerance = 1e-4)
  result_sds <- exp(result$theta_est[c(1:4, 6:9)])
  expected_sds <- sqrt(c(122.64, 45.1705, 21.411, 126.14, 82.7175, 31.3308, 44.6841, 234.52))
  expect_equal(result_sds, expected_sds, tolerance = 1e-4)
  result_rho <- map_to_cor(result$theta_est[c(5, 10)])
  expected_rho <- c(0.08074, 0.2097)
  expect_equal(result_rho, expected_rho, tolerance = 1e-3)
})

## spatial exponential ----

test_that("h_mmrm_tmb works with sp_exp covariance structure and ML", {
  formula <- FEV1 ~ sp_exp(VISITN | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_sp_exp_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3875.95353357)
  expect_equal(as.numeric(result$beta_est[1]), 42.3252, tolerance = 1e-4)
  expect_equal(plogis(result$theta_est[2])^2, 0.1805, tolerance = 1e-3)
  expect_equal(exp(result$theta_est[1]), 88.7005, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with sp_exp covariance structure and ML(2-dimension)", {
  formula <- FEV1 ~ sp_exp(VISITN, VISITN2 | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = FALSE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_sp_exp2_ml.txt for the source of numbers.
  expect_equal(deviance(result), 3894.94943409)
  expect_equal(as.numeric(result$beta_est[1]), 42.2203, tolerance = 1e-4)
  expect_equal(plogis(result$theta_est[2])^dist(fev_data[c(2, 4), c("VISITN", "VISITN2")])[1], 0.1375, tolerance = 1e-3)
  expect_equal(exp(result$theta_est[1]), 87.6472, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with sp_exp covariance structure and REML", {
  formula <- FEV1 ~ sp_exp(VISITN | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_sp_exp_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3875.49946734)
  expect_equal(as.numeric(result$beta_est[1]), 42.3254, tolerance = 1e-4)
  expect_equal(plogis(result$theta_est[2])^2, 0.1823, tolerance = 1e-3)
  expect_equal(exp(result$theta_est[1]), 88.9768, tolerance = 1e-3)
})

test_that("h_mmrm_tmb works with sp_exp covariance structure and REML(2-dimension)", {
  formula <- FEV1 ~ sp_exp(VISITN, VISITN2 | USUBJID)
  result <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE)
  expect_class(result, "mmrm_tmb")
  # See design/SAS/sas_sp_exp2_reml.txt for the source of numbers.
  expect_equal(deviance(result), 3894.60123182)
  expect_equal(as.numeric(result$beta_est[1]), 42.2202, tolerance = 1e-4)
  expect_equal(plogis(result$theta_est[2])^dist(fev_data[c(2, 4), c("VISITN", "VISITN2")])[1], 0.1393, tolerance = 1e-3)
  expect_equal(exp(result$theta_est[1]), 87.8870, tolerance = 1e-3)
})

## misc ----

test_that("fit_mmrm also works with character ID variable", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  data <- fev_data
  data$USUBJID <- as.character(data$USUBJID) # nolint
  result <- expect_silent(fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)), reml = TRUE))
  expected <- expect_silent(fit_mmrm(formula, data, weights = rep(1, nrow(data)), reml = TRUE))
  expect_identical(result$beta_est, expected$beta_est)
})

test_that("fit_mmrm saves data name in call element as expected", {
  formula <- FEV1 ~ us(AVISIT | USUBJID)
  fit <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)))
  saved_call <- fit$call
  expect_class(saved_call, "call")
  expect_identical(saved_call$data, "fev_data")
})

test_that("fit_mmrm works even when time point variable has unused factor levels", {
  tmp_data <- fev_data
  tmp_data$AVISIT <- factor(tmp_data$AVISIT, levels = c("SCREENING", "VIS1", "VIS2", "VIS3", "VIS4"))
  expect_message(
    result <- fit_mmrm(
      FEV1 ~ FEV1_BL + RACE + us(AVISIT | USUBJID),
      data = tmp_data,
      weights = rep(1, nrow(tmp_data))
    ),
    "In AVISIT there are dropped visits: SCREENING"
  )
  expect_class(result, "mmrm_tmb")
  expect_identical(
    rownames(VarCorr(result)),
    c("VIS1", "VIS2", "VIS3", "VIS4")
  )
})

test_that("fit_mmrm works if we keep the unused factor levels for specific covariance structure", {
  tmp_data <- fev_data
  tmp_data$AVISIT <- factor(tmp_data$AVISIT, levels = c("SCREENING", "VIS1", "VIS2", "VIS3", "VIS4"))

  expect_silent(result <- fit_mmrm(
    FEV1 ~ FEV1_BL + RACE + ar1(AVISIT | USUBJID),
    data = tmp_data,
    weights = rep(1, nrow(tmp_data)),
    control = mmrm_control(drop_visit_levels = FALSE)
  ))
  expect_class(result, "mmrm_tmb")
  expect_identical(
    rownames(VarCorr(result)),
    c("SCREENING", "VIS1", "VIS2", "VIS3", "VIS4")
  )
})

test_that("fit_mmrm warns if we keep the unused factor levels for unstructured covariance", {
  tmp_data <- fev_data
  tmp_data$AVISIT <- factor(tmp_data$AVISIT, levels = c("SCREENING", "VIS1", "VIS2", "VIS3", "VIS4"))

  expect_warning(
    result <- fit_mmrm(
      FEV1 ~ FEV1_BL + RACE + us(AVISIT | USUBJID),
      data = tmp_data,
      weights = rep(1, nrow(tmp_data)),
      control = mmrm_control(drop_visit_levels = FALSE)
    ),
    "Model convergence problem: hessian is singular, theta_vcov not available"
  )
  expect_class(result, "mmrm_tmb")
  expect_identical(
    rownames(VarCorr(result)),
    c("SCREENING", "VIS1", "VIS2", "VIS3", "VIS4")
  )
})

test_that("fit_mmrm works with below full rank original design matrix by default", {
  formula <- FEV1 ~ RACE + SEX + SEX2 + ARMCD * AVISIT + us(AVISIT | USUBJID)
  dat <- fev_data
  dat$SEX2 <- dat$SEX # nolint
  result <- expect_silent(fit_mmrm(formula, dat, weights = rep(1, nrow(dat))))
  expect_match(names(which(result$tmb_data$x_cols_aliased)), "SEX2")
})
