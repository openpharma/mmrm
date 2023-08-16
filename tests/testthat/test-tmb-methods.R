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

test_that("fitted give same result compared to nlme", {
  skip_if_not_installed("nlme")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  # In this test, subject IDs are ordered.
  expect_true(all(diff(as.integer(data_full$USUBJID)) >= 0))
  fit <- mmrm(formula = formula, data = data_full, method = "Satterthwaite")
  fit_gls <- nlme::gls(FEV1 ~ ARMCD, data_full, correlation = nlme::corAR1(form = ~ VISITN | USUBJID))
  expect_identical(fitted(fit_gls), fitted(fit), tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("fitted give same result compared to nlme if the order is changed", {
  skip_if_not_installed("nlme")
  formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
  data_full <- fev_data[complete.cases(fev_data), ]
  new_order <- sample(seq_len(nrow(data_full)))
  fit <- mmrm(formula = formula, data = data_full[new_order, ], method = "Satterthwaite")
  fit_gls <- nlme::gls(FEV1 ~ ARMCD, data_full[new_order, ], correlation = nlme::corAR1(form = ~ VISITN | USUBJID))
  expect_identical(fitted(fit_gls), fitted(fit)[new_order], tolerance = 1e-4, ignore_attr = TRUE)
})

# h_get_prediction ----

test_that("h_get_prediction works", {
  fit <- get_mmrm()
  res <- expect_silent(h_get_prediction(fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov))
  expect_true(length(res$index) == 197)
})

test_that("h_get_prediction works for partial data", {
  fit <- get_mmrm()
  data <- fev_data[c(1:4, 97:100), ]
  full_frame <- model.frame(fit,
    data = data,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  res <- expect_silent(h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov))
  expect_true(length(res$index) == 2)
  expect_true(length(res$covariance) == 2)
  expect_identical(length(res$index[[1]]), nrow(res$covariance[[1]]))
})

# predict -----

test_that("predict works for old patient, new visit", {
  fit <- get_mmrm()
  expect_silent(predict(fit, fev_data[c(1, 4), ]))
  expect_silent(predict(fit, fev_data[c(2, 3), ]))
  expect_silent(predict(fit, fev_data[c(1:4), ]))
})

test_that("predict works for only fit values without response", {
  object <- get_mmrm()
  m <- stats::model.matrix(
    object$formula_parts$model_formula,
    model.frame(object, data = fev_data, include = "response_var", na.action = "na.pass")
  )
  fev_data_no_y <- fev_data
  fev_data_no_y$FEV1 <- NA_real_
  y_pred <- expect_silent(predict(object, fev_data_no_y))
  y_hat <- as.vector(m %*% object$beta_est)
  expect_equal(y_pred, y_hat, ignore_attr = TRUE)
})

test_that("predict works for only fit values with response", {
  object <- get_mmrm()
  y_pred <- expect_silent(predict(object, fev_data))
  expect_equal(y_pred[!is.na(fev_data$FEV1)], object$tmb_data$y_vector, ignore_attr = TRUE)
  expect_numeric(y_pred[is.na(fev_data$FEV1)])
})

test_that("predict warns on aliased variables", {
  new_fev_data <- rbind(
    fev_data,
    fev_data %>%
      dplyr::filter(ARMCD == fev_data$ARMCD[1], AVISIT == "VIS1") %>%
      dplyr::mutate(AVISIT = "VIS5", FEV1 = rnorm(dplyr::n(), mean = 45, sd = 5))
  )
  fit <- mmrm(
    formula = FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = new_fev_data
  )
  expect_warning(predict(fit), "In fitted object there are co-linear variables and therefore dropped terms")
})

test_that("predict will return on correct order", {
  new_order <- sample(seq_len(nrow(fev_data)))
  fit <- get_mmrm()
  fev_data2 <- fev_data
  fev_data2$FEV1 <- NA_real_
  predicted <- predict(fit, newdata = fev_data2[new_order, ])

  m <- stats::model.matrix(
    fit$formula_parts$model_formula,
    model.frame(fit, data = fev_data2[new_order, ], include = "response_var", na.action = "na.pass")
  )

  expect_identical(
    predicted,
    (m %*% fit$beta_est)[, 1],
    tolerance = 1e-8
  )
})

test_that("predict will return NA if data contains NA in covariates", {
  new_order <- sample(seq_len(nrow(fev_data)))
  fit <- get_mmrm()
  fev_data2 <- fev_data
  fev_data2$FEV1 <- NA_real_
  fev_data2$SEX[1:20] <- NA
  predicted <- predict(fit, newdata = fev_data2[new_order, ], se.fit = TRUE, interval = "confidence")

  m <- stats::model.matrix(
    fit$formula_parts$model_formula,
    model.frame(fit, data = fev_data2[new_order, ], include = "response_var", na.action = "na.pass")
  )

  expect_identical(
    predicted[, "fit"],
    (m %*% fit$beta_est)[, 1],
    tolerance = 1e-8
  )
  na_index <- which(new_order <= 20)
  expect_identical(
    predicted[na_index, ],
    matrix(NA_real_, nrow = 20, ncol = 4, dimnames = list(row.names(m)[na_index], c("fit", "se", "lwr", "upr")))
  )
})

## integration test with SAS ----

test_that("predict gives same result with sas in unstructured satterthwaite/Kenward-Roger", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  full_frame <- model.frame(fit,
    data = fev_data,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  sas_res_p <- c(44.9753553724207, 41.4074753229983)
  sas_res_p_sd <- c(7.07537882030293, 4.72199122235202)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-3)

  sas_res_p <- c(44.9753553724207, 41.4074753229983)
  sas_res_p_sd <- c(7.07537882030293, 4.72199122235202)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov_adj)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-3)
})

test_that("predict gives same result with sas in toep satterthwaite", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID), data = fev_data)
  full_frame <- model.frame(fit,
    data = fev_data,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  sas_res_p <- c(49.2972533971016, 32.5585487765097)
  sas_res_p_sd <- c(7.89495102841021, 7.32982693449642)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-3)
})

test_that("predict gives same result with sas in ar1 satterthwaite/kenward-roger", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  full_frame <- model.frame(fit,
    data = fev_data,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  sas_res_p <- c(42.7404650408987, 34.8347780646782)
  sas_res_p_sd <- c(8.469966361884, 7.88346546930503)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-3)

  sas_res_p <- c(42.7404650408987, 34.8347780646782)
  sas_res_p_sd <- c(8.50085016679827, 7.91455389252041)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov_adj)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-2)
})

test_that("predict gives same result with sas in cs satterthwaite/kenward-roger", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  full_frame <- model.frame(fit,
    data = fev_data,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  sas_res_p <- c(44.0595030539703, 44.0595030539703)
  sas_res_p_sd <- c(9.10525401644359, 9.10525401644359)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-3)

  sas_res_p <- c(44.0595030539703, 44.0595030539703)
  sas_res_p_sd <- c(9.13792825096229, 9.13792825096229)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov_adj)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-2)
})


test_that("predict gives same result with sas in sp_exp satterthwaite/kenward-roger", {
  fit <- mmrm(FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID), data = fev_data, method = "Kenward-Roger")
  full_frame <- model.frame(fit,
    data = fev_data,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  sas_res_p <- c(43.0487253094136, 41.7658999941086)
  sas_res_p_sd <- c(8.70129696071474, 8.78510718323017)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-3)

  sas_res_p <- c(43.0487253094136, 41.7658999941086)
  sas_res_p_sd <- c(8.73761421183867, 8.82021391504478)
  res <- h_get_prediction(tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov_adj)
  expect_equal(sas_res_p, res$prediction[c(1, 3), 1], tolerance = 1e-3)
  expect_equal(sas_res_p_sd, sqrt(res$prediction[c(1, 3), 3]), tolerance = 1e-2)
})

# h_construct_model_frame_inputs ----

test_that("h_construct_model_frame_inputs works with all columns", {
  object <- get_mmrm_tmb()

  result <-
    expect_silent(
      h_construct_model_frame_inputs(
        formula = object,
        data = object$data |> head(),
        include = c("subject_var", "visit_var", "group_var", "response_var"),
      )
    )
  expect_equal(
    result$formula,
    FEV1 ~ RACE + USUBJID + AVISIT,
    ignore_attr = TRUE
  )
  expect_equal(
    result$formula_full,
    FEV1 ~ RACE + USUBJID + AVISIT,
    ignore_attr = TRUE
  )
})

test_that("h_construct_model_frame_inputs works with response var selected", {
  object <- get_mmrm_tmb()
  result <-
    expect_silent(
      h_construct_model_frame_inputs(
        formula = object,
        data = object$data |> head(),
        include = "response_var",
      )
    )
  expect_equal(
    result$formula,
    FEV1 ~ RACE,
    ignore_attr = TRUE
  )
  expect_equal(
    result$formula_full,
    FEV1 ~ RACE + USUBJID + AVISIT,
    ignore_attr = TRUE
  )
})

test_that("h_construct_model_frame_inputs works with include=NULL", {
  object <- get_mmrm_tmb()
  result <-
    expect_silent(
      h_construct_model_frame_inputs(
        formula = object,
        data = object$data |> head(),
        include = NULL,
      )
    )
  expect_equal(
    result$formula,
    ~RACE,
    ignore_attr = TRUE
  )
  expect_equal(
    result$formula_full,
    FEV1 ~ RACE + USUBJID + AVISIT,
    ignore_attr = TRUE
  )
})


# model.frame ----

test_that("model.frame works as expected with defaults", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object, include = c("response_var", "visit_var")))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works as expected with includes", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object, include = c("response_var")))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame returns full model frame if requested", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.frame(object, include = c("response_var", "visit_var", "subject_var", "group_var")))
  expect_data_frame(result, nrows = length(object$tmb_data$y_vector))
  expect_named(result, c("FEV1", "RACE", "AVISIT", "USUBJID"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works if variable transformed", {
  fit1 <- get_mmrm_transformed()
  result <- expect_silent(model.frame(fit1, include = c("response_var", "visit_var")))
  expect_data_frame(result, nrows = length(fit1$tmb_data$y_vector))
  expect_named(result, c("FEV1", "log(FEV1_BL)", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works for new data", {
  fit1 <- get_mmrm_transformed()
  result <- expect_silent(model.frame(
    fit1,
    data = fev_data[complete.cases(fev_data), ][1:20, ],
    include = c("response_var", "visit_var")
  ))
  expect_data_frame(result, nrows = 20L)
  expect_named(result, c("FEV1", "log(FEV1_BL)", "AVISIT"))
  expect_class(attr(result, "terms"), "terms")
})

test_that("model.frame works if input x does not contain NA, y contains but not included", {
  fit1 <- get_mmrm_transformed()
  expect_silent(
    model.frame(fit1, data = na.omit(fit1$data), na.action = "na.fail")
  )
})

test_that("model.frame fails if y contains NA and is included", {
  fit1 <- get_mmrm_transformed()
  expect_error(
    model.frame(fit1, na.action = "na.fail", include = "response_var")
  )
})

test_that("model.frame makes the levels match", {
  fit1 <- get_mmrm()
  fev_data2 <- fev_data
  # changes the levels
  levels(fev_data2$AVISIT) <- c("VIS3", "VIS2", "VIS4", "VIS1")
  out_frame <- expect_silent(
    model.frame(fit1, data = fev_data2)
  )
  expect_identical(levels(fev_data$AVISIT), levels(out_frame$AVISIT))
})

test_that("model.frame do not care about subject levels", {
  fit1 <- get_mmrm()
  fev_data2 <- fev_data
  fev_data2$USUBJID <- sprintf("%s_TEST", fev_data2$USUBJID)
  out_frame <- expect_silent(
    model.frame(fit1, data = fev_data2, include = "subject_var")
  )
  expect_identical(
    # first remove missing obs from input data
    na.omit(fev_data2[all.vars(fit1$formula$formula)])$USUBJID,
    out_frame$USUBJID
  )
})

test_that("model.frame include all specified variables", {
  fit1 <- get_mmrm_group()
  out_frame <- expect_silent(
    model.frame(fit1, data = fev_data, include = "group_var")
  )
  expect_identical(colnames(out_frame), "ARMCD") # formula already contains "ARMCD"

  out_frame <- expect_silent(
    model.frame(fit1, data = fev_data, include = "visit_var")
  )
  expect_identical(colnames(out_frame), c("ARMCD", "AVISIT"))

  out_frame <- expect_silent(
    model.frame(fit1, data = fev_data, include = "subject_var")
  )
  expect_identical(colnames(out_frame), c("ARMCD", "USUBJID"))

  out_frame <- expect_silent(
    model.frame(fit1, na.action = "na.pass", data = fev_data, include = "response_var")
  )
  expect_identical(colnames(out_frame), c("FEV1", "ARMCD"))
})

test_that("model.frame with character reference will return factors", {
  fev_data2 <- fev_data
  fev_data2$ARMCD <- as.character(fev_data2$ARMCD)
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data2)
  new_data <- subset(fev_data2, ARMCD == "TRT")
  new_frame <- expect_silent(model.frame(fit, data = new_data, include = c("subject_var", "visit_var", "response_var")))
  expect_identical(levels(new_frame$ARMCD), c("PBO", "TRT"))
  expect_silent(h_mmrm_tmb_data(
    fit$formula_parts, new_frame,
    weights = rep(1, nrow(new_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  ))
  # If we don't have the factorization in model.frame
  new_frame2 <- new_frame
  new_frame2$ARMCD <- as.character(new_frame2$ARMCD)
  expect_error(h_mmrm_tmb_data(
    fit$formula_parts, new_frame2,
    weights = rep(1, nrow(new_frame2)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  ), "contrasts can be applied only to factors")
})

# model.matrix ----

test_that("model.matrix works as expected with defaults", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.matrix(object))
  expect_matrix(result, nrows = length(object$tmb_data$y_vector))
  expect_equal(
    colnames(result),
    c("(Intercept)", "RACEBlack or African American", "RACEWhite")
  )
})

test_that("model.matrix works as expected with includes", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.matrix(object, include = c("visit_var")))
  expect_matrix(result, nrows = length(object$tmb_data$y_vector))
  expect_equal(
    colnames(result),
    c("(Intercept)", "RACEBlack or African American", "RACEWhite", "AVISITVIS2", "AVISITVIS3", "AVISITVIS4")
  )
})

test_that("model.matrix returns full model frame if requested", {
  object <- get_mmrm_tmb()
  result <- expect_silent(model.matrix(object, include = c("visit_var", "subject_var", "group_var")))
  expect_matrix(result, nrows = length(object$tmb_data$y_vector))
  # Expecting many columns for covariates and dummy cols for USUBJID.
  expect_equal(
    length(colnames(result)),
    205L
  )
})

test_that("model.matrix works if variable transformed", {
  fit1 <- get_mmrm_transformed()
  result <- expect_silent(model.matrix(fit1, include = c("visit_var")))
  expect_matrix(result, nrows = length(fit1$tmb_data$y_vector))
  expect_equal(
    colnames(result),
    c("(Intercept)", "log(FEV1_BL)", "AVISITVIS2", "AVISITVIS3", "AVISITVIS4")
  )
})

test_that("model.matrix works for new data", {
  fit1 <- get_mmrm_transformed()
  result <- expect_silent(model.matrix(
    fit1,
    data = fev_data[complete.cases(fev_data), ][1:20, ],
    include = c("visit_var")
  ))
  expect_matrix(result, nrows = 20L)
  expect_equal(
    colnames(result),
    c("(Intercept)", "log(FEV1_BL)", "AVISITVIS2", "AVISITVIS3", "AVISITVIS4")
  )
})

test_that("model.matrix include all specified variables", {
  fit1 <- get_mmrm_group()
  out_frame <- expect_silent(
    model.matrix(fit1, data = fev_data, include = "group_var")
  )
  expect_identical(colnames(out_frame), c("(Intercept)", "ARMCDTRT")) # formula already contains "ARMCD"

  out_frame <- expect_silent(
    model.matrix(fit1, data = fev_data, include = "visit_var")
  )
  expect_identical(colnames(out_frame), c("(Intercept)", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3", "AVISITVIS4"))
})

# terms ----

test_that("terms works as expected with defaults", {
  object <- get_mmrm_tmb()
  result <- expect_silent(terms(object))
  expect_equal(
    result,
    FEV1 ~ RACE,
    ignore_attr = TRUE
  )
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
  result_normal <- residuals(object, type = "normalized")
  expect_equal(head(result_normal, 5), c(-0.4943, -2.5698, -0.9613, 0.0046, 1.1645), tolerance = 1e-4)
  result_pearson <- residuals(object, type = "normalized")
  expect_equal(head(result_pearson, 5), c(-0.4944, -2.5699, -0.9613, 0.0046, 1.1645), tolerance = 1e-4)
})

test_that("pearson residuals helper function works as expected", {
  object <- get_mmrm()
  resid_response <- residuals(object, type = "response")
  result_pearson <- h_residuals_pearson(object)
  expect_double(result_pearson, len = length(object$tmb_data$y_vector))
  expect_equal(tail(result_pearson, 5), c(2.22057, 1.79050, 0.53322, 0.87243, 0.70477), tolerance = 1e-4)
})

test_that("normalized residuals helper function works as expected", {
  object <- get_mmrm()
  result_norm <- h_residuals_normalized(object)
  expect_double(result_norm, len = length(object$tmb_data$y_vector))
  expect_equal(tail(result_norm, 5), c(1.99092, 1.49689, 0.53322, 0.71055, 0.56152), tolerance = 1e-4)
})

test_that("response residuals helper function works as expected", {
  object <- get_mmrm()
  result_rsp <- h_residuals_response(object)
  expect_double(result_rsp, len = length(object$tmb_data$y_vector))
  expected <- component(object, "y_vector") - as.vector(fitted(object))
  expect_equal(result_rsp, expected)
})
