#' Methods for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM object.
#' @param x (`mmrm_tmb`)\cr same as `object`.
#' @param formula (`mmrm_tmb`)\cr same as `object`.
#' @param complete (`flag`)\cr whether to include potential non-estimable
#'   coefficients.
#' @param ... mostly not used; Exception is `model.matrix()` passing `...` to the default method.
#' @return Depends on the method, see Functions.
#'
#' @name mmrm_tmb_methods
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' object <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)))
NULL

#' @describeIn mmrm_tmb_methods obtains the estimated coefficients.
#' @importFrom stats coef
#' @exportS3Method
#' @examples
#' # Estimated coefficients:
#' coef(object)
coef.mmrm_tmb <- function(object, complete = TRUE, ...) {
  assert_flag(complete)
  nm <- if (complete) "beta_est_complete" else "beta_est"
  component(object, name = nm)
}

#' @describeIn mmrm_tmb_methods obtains the fitted values.
#' @importFrom stats fitted
#' @exportS3Method
#' @examples
#' # Fitted values:
#' fitted(object)
fitted.mmrm_tmb <- function(object, ...) {
  fitted_col <- component(object, "x_matrix") %*% component(object, "beta_est")
  fitted_col[, 1L, drop = TRUE]
}

#' @describeIn mmrm_tmb_methods predict conditional means for new data;
#'  optionally with standard errors and confidence or prediction intervals.
#'  Returns a vector of predictions if `se.fit == FALSE` and
#'  `interval == "none"`; otherwise it returns a data.frame with multiple
#'  columns and one row per input data row.
#' @importFrom stats predict
#' @param newdata (`data.frame`)\cr object in which to look for variables with which to predict.
#' @param se.fit (`flag`)\cr indicator if standard errors are required.
#' @param interval (`string`)\cr type of interval calculation. Can be abbreviated.
#' @param level (`number`)\cr tolerance/confidence level.
#' @param n_sim (`integr`)\cr number of replications to calculate prediction interval.
#' @exportS3Method
#' @examples
#' predict(object, newdata = fev_data)
predict.mmrm_tmb <- function(
    object, newdata, se.fit = FALSE, # nolint
    interval = c("none", "confidence", "prediction"), level = 0.95,
    n_sim = 1000L, ...) {
  if (missing(newdata)) {
    newdata <- object$tmb_data$data
  }
  assert_data_frame(newdata)
  orig_row_names <- row.names(newdata)
  assert_flag(se.fit)
  assert_number(level, lower = 0, upper = 1)
  assert_int(n_sim, lower = 1)
  interval <- match.arg(interval)
  # make sure new data has the same levels as original data
  full_frame <- model.frame(
    object,
    data = newdata,
    include = c("subject_var", "visit_var", "group_var", "response_var"),
    na.action = "na.pass"
  )
  tmb_data <- h_mmrm_tmb_data(
    object$formula_parts, full_frame,
    weights = rep(1, nrow(full_frame)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE
  )
  if (any(object$tmb_data$x_cols_aliased)) {
    warning(
      "In fitted object there are co-linear variables and therefore dropped terms, ",
      "and this could lead to incorrect prediction on new data."
    )
  }
  colnames <- names(Filter(isFALSE, object$tmb_data$x_cols_aliased))
  tmb_data$x_matrix <- tmb_data$x_matrix[, colnames, drop = FALSE]
  predictions <- h_get_prediction(
    tmb_data, object$theta_est, object$beta_est, component(object, "beta_vcov")
  )$prediction
  res <- cbind(fit = rep(NA_real_, nrow(newdata)))
  new_order <- match(row.names(tmb_data$full_frame), orig_row_names)
  res[new_order, "fit"] <- predictions[, 1]
  se <- switch(interval,
    "confidence" = sqrt(predictions[, 2]),
    "prediction" = sqrt(h_get_prediction_variance(object, n_sim, tmb_data)),
    "none" = NULL
  )
  if (se.fit && interval != "none") {
    res <- cbind(
      res,
      se = NA_real_
    )
    res[new_order, "se"] <- se
  }
  if (interval != "none") {
    alpha <- 1 - level
    z <- stats::qnorm(1 - alpha / 2) * res[, "se"]
    res <- cbind(
      res,
      lwr = res[, "fit"] - z,
      upr = res[, "fit"] + z
    )
  }
  # Use original names.
  row.names(res) <- orig_row_names
  if (ncol(res) == 1) {
    res <- res[, "fit"]
  }
  return(res)
}

#' Get Prediction
#' @description Get predictions with given `data`, `theta`, `beta`, `beta_vcov`.
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr object.
#' @param theta (`numeric`)\cr theta value.
#' @param beta (`numeric`)\cr beta value.
#' @param beta_vcov (`matrix`)\cr beta_vcov matrix.
#'
#' @keywords internal
h_get_prediction <- function(tmb_data, theta, beta, beta_vcov) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_numeric(theta)
  n_beta <- ncol(tmb_data$x_matrix)
  assert_numeric(beta, finite = TRUE, any.missing = FALSE, len = n_beta)
  assert_matrix(beta_vcov, mode = "numeric", any.missing = FALSE, nrows = n_beta, ncols = n_beta)
  .Call(`_mmrm_predict`, PACKAGE = "mmrm", tmb_data, theta, beta, beta_vcov)
}

#' Get Prediction Variance
#' @description Get prediction variance with given fit, `tmb_data` with the Monte Carlo sampling method.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#' @param n_sim (`integer`)\cr number of samples.
#' @param tmb_data (`mmrm_tmb_data`)\cr object.
#'
#' @keywords internal
h_get_prediction_variance <- function(object, n_sim, tmb_data) {
  assert_class(object, "mmrm_tmb")
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_int(n_sim)
  theta_chol <- chol(object$theta_vcov)
  n_theta <- length(object$theta_est)
  res <- replicate(n_sim, {
    z <- stats::rnorm(n = n_theta)
    theta_sample <- object$theta_est + theta_chol %*% z
    cond_beta_results <- object$tmb_object$report(theta_sample)
    beta_mean <- cond_beta_results$beta
    beta_cov <- cond_beta_results$beta_vcov
    h_get_prediction(tmb_data, theta_sample, beta_mean, beta_cov)$prediction
  })
  mean_of_var <- rowMeans(res[, 1, ])
  var_of_mean <- apply(res[, 3, ], 1, stats::var)
  mean_of_var + var_of_mean
}


#' @describeIn mmrm_tmb_methods obtains the model frame.
#' @param data (`data.frame`)\cr object in which to construct the frame.
#' @param include (`character`)\cr names of variable types to include.
#'   Must be `NULL` or one or more of `c("subject_var", "visit_var", "group_var", "response_var")`.
#' @param full (`flag`)\cr indicator whether to return full model frame (deprecated).
#' @param na.action (`string`)\cr na action.
#' @importFrom stats model.frame
#' @exportS3Method
#'
#' @details
#' `include` argument controls the variables the returned model frame will include.
#' Possible options are "response_var", "subject_var", "visit_var" and "group_var", representing the
#' response variable, subject variable, visit variable or group variable.
#' `character` values in new data will always be factorized according to the data in the fit
#' to avoid mismatched in levels or issues in `model.matrix`.
#'
#' @examples
#' # Model frame:
#' model.frame(object)
#' model.frame(object, include = "subject_var")
model.frame.mmrm_tmb <- function(formula, data, include = c("subject_var", "visit_var", "group_var", "response_var"),
                                 full, na.action = "na.omit", ...) { # nolint
  # Construct updated formula and data arguments.
  lst_formula_and_data <-
    h_construct_model_frame_inputs(
      formula = formula,
      data = data,
      include = include,
      full = full
    )

  # Construct data frame to return to users.
  ret <-
    stats::model.frame(
      formula = lst_formula_and_data$formula,
      data = lst_formula_and_data$data,
      na.action = na.action
    )
  # We need the full formula obs, so recalculating if not already full.
  ret_full <- if (lst_formula_and_data$is_full) {
    ret
  } else {
    stats::model.frame(
      formula = lst_formula_and_data$formula_full,
      data = lst_formula_and_data$data,
      na.action = na.action
    )
  }

  # Lastly, subsetting returned data frame to only include obs utilized in model.
  ret[rownames(ret) %in% rownames(ret_full), , drop = FALSE]
}


#' Construction of Model Frame Formula and Data Inputs
#'
#' @description
#' Input formulas are converted from mmrm-style to a style compatible
#' with default [stats::model.frame()] and [stats::model.matrix()] methods.
#'
#' The full formula is returned so we can construct, for example, the
#' `model.frame()` including all columns as well as the requested subset.
#' The full set is used to identify rows to include in the reduced model frame.
#'
#' @param formula (`mmrm`)\cr mmrm fit object.
#' @param data optional data frame that will be
#'   passed to `model.frame()` or `model.matrix()`
#' @param include (`character`)\cr names of variable to include
#' @param full (`flag`)\cr indicator whether to return full model frame (deprecated).
#'
#' @return named list with four elements:
#' - `"formula"`: the formula including the columns requested in the `include=` argument.
#' - `"formula_full"`: the formula including all columns
#' - `"data"`: a data frame including all columns where factor and
#'   character columns have been processed with [h_factor_ref()].
#' - `"is_full"`: a logical scalar indicating if the formula and
#'   full formula are identical
#' @keywords internal
h_construct_model_frame_inputs <- function(formula, data, include,
                                           include_choice = c("subject_var", "visit_var", "group_var", "response_var"),
                                           full) {
  if (!missing(full) && identical(full, TRUE)) {
    lifecycle::deprecate_warn("0.3", "model.frame.mmrm_tmb(full)")
    include <- include_choice
  }

  assert_class(formula, classes = "mmrm_tmb")
  assert_subset(include, include_choice)
  if (missing(data)) {
    data <- formula$tmb_data$data
  }
  assert_data_frame(data)

  drop_response <- !"response_var" %in% include
  add_vars <- unlist(formula$formula_parts[include])
  new_formula <- h_add_terms(formula$formula_parts$model_formula, add_vars, drop_response)

  drop_response_full <- !"response_var" %in% include_choice
  add_vars_full <- unlist(formula$formula_parts[include_choice])
  new_formula_full <-
    h_add_terms(formula$formula_parts$model_formula, add_vars_full, drop_response_full)

  # Update data based on the columns in the full formula return.
  all_vars <- all.vars(new_formula_full)
  assert_names(colnames(data), must.include = all_vars)
  full_frame <- formula$tmb_data$full_frame
  for (v in setdiff(all_vars, formula$formula_parts$subject_var)) {
    if (is.factor(full_frame[[v]]) || is.character(full_frame[[v]])) {
      data[[v]] <- h_factor_ref(data[[v]], full_frame[[v]])
    }
  }

  # Return list with updated formula, full formula, data, and full formula flag.
  list(
    formula = new_formula,
    formula_full = new_formula_full,
    data = data,
    is_full = setequal(include, include_choice)
  )
}

#' @describeIn mmrm_tmb_methods obtains the model matrix.
#' @exportS3Method
#'
#' @examples
#' # Model matrix:
#' model.matrix(object)
model.matrix.mmrm_tmb <- function(object, data, include = NULL, ...) { # nolint
  # Construct updated formula and data arguments.
  assert_subset(include, c("subject_var", "visit_var", "group_var"))
  lst_formula_and_data <-
    h_construct_model_frame_inputs(formula = object, data = data, include = include)

  # Construct matrix to return to users.
  ret <-
    stats::model.matrix(
      object = lst_formula_and_data$formula,
      data = lst_formula_and_data$data,
      ...
    )

  # We need the full formula obs, so recalculating if not already full.
  ret_full <- if (lst_formula_and_data$is_full) {
    ret
  } else {
    stats::model.matrix(
      object = lst_formula_and_data$formula_full,
      data = lst_formula_and_data$data,
      ...
    )
  }

  # Subset data frame to only include obs utilized in model.
  ret[rownames(ret) %in% rownames(ret_full), , drop = FALSE]
}

#' @describeIn mmrm_tmb_methods obtains the terms object.
#' @importFrom stats model.frame
#' @exportS3Method
#'
#' @examples
#' # terms:
#' terms(object)
#' terms(object, include = "subject_var")
terms.mmrm_tmb <- function(x, include = "response_var", ...) { # nolint
  # Construct updated formula and data arguments.
  lst_formula_and_data <-
    h_construct_model_frame_inputs(
      formula = x,
      include = include
    )

  # Use formula method for `terms()` to construct the mmrm terms object.
  stats::terms(
    x = lst_formula_and_data$formula,
    data = lst_formula_and_data$data
  )
}


#' @describeIn mmrm_tmb_methods obtains the attained log likelihood value.
#' @importFrom stats logLik
#' @exportS3Method
#' @examples
#' # Log likelihood given the estimated parameters:
#' logLik(object)
logLik.mmrm_tmb <- function(object, ...) {
  -component(object, "neg_log_lik")
}

#' @describeIn mmrm_tmb_methods obtains the used formula.
#' @importFrom stats formula
#' @exportS3Method
#' @examples
#' # Formula which was used:
#' formula(object)
formula.mmrm_tmb <- function(x, ...) {
  x$formula_parts$formula
}

#' @describeIn mmrm_tmb_methods obtains the variance-covariance matrix estimate
#'   for the coefficients.
#' @importFrom stats vcov
#' @exportS3Method
#' @examples
#' # Variance-covariance matrix estimate for coefficients:
#' vcov(object)
vcov.mmrm_tmb <- function(object, complete = TRUE, ...) {
  assert_flag(complete)
  nm <- if (complete) "beta_vcov_complete" else "beta_vcov"
  component(object, name = nm)
}

#' @describeIn mmrm_tmb_methods obtains the variance-covariance matrix estimate
#'   for the residuals.
#' @param sigma cannot be used (this parameter does not exist in MMRM).
#' @importFrom nlme VarCorr
#' @export VarCorr
#' @aliases VarCorr
#' @exportS3Method
#' @examples
#' # Variance-covariance matrix estimate for residuals:
#' VarCorr(object)
VarCorr.mmrm_tmb <- function(x, sigma = NA, ...) { # nolint
  assert_scalar_na(sigma)

  component(x, name = "varcor")
}

#' @describeIn mmrm_tmb_methods obtains the deviance, which is defined here
#'   as twice the negative log likelihood, which can either be integrated
#'   over the coefficients for REML fits or the usual one for ML fits.
#' @importFrom stats deviance
#' @exportS3Method
#' @examples
#' # REML criterion (twice the negative log likelihood):
#' deviance(object)
deviance.mmrm_tmb <- function(object, ...) {
  2 * component(object, "neg_log_lik")
}

#' @describeIn mmrm_tmb_methods obtains the Akaike Information Criterion,
#'   where the degrees of freedom are the number of variance parameters (`n_theta`).
#'   If `corrected`, then this is multiplied with `m / (m - n_theta - 1)` where
#'   `m` is the number of observations minus the number of coefficients, or
#'   `n_theta + 2` if it is smaller than that \insertCite{hurvich1989regression,burnham1998practical}{mmrm}.
#' @param corrected (`flag`)\cr whether corrected AIC should be calculated.
#' @param k (`number`)\cr the penalty per parameter to be used; default `k = 2`
#'   is the classical AIC.
#' @importFrom stats AIC
#' @exportS3Method
#' @examples
#' # AIC:
#' AIC(object)
#' AIC(object, corrected = TRUE)
#' @references
#' - \insertRef{hurvich1989regression}{mmrm}
#' - \insertRef{burnham1998practical}{mmrm}
AIC.mmrm_tmb <- function(object, corrected = FALSE, ..., k = 2) {
  # nolint
  assert_flag(corrected)
  assert_number(k, lower = 1)

  n_theta <- length(component(object, "theta_est"))
  df <- if (!corrected) {
    n_theta
  } else {
    n_obs <- length(component(object, "y_vector"))
    n_beta <- length(component(object, "beta_est"))
    m <- max(n_theta + 2, n_obs - n_beta)
    n_theta * (m / (m - n_theta - 1))
  }

  2 * component(object, "neg_log_lik") + k * df
}

#' @describeIn mmrm_tmb_methods obtains the Bayesian Information Criterion,
#'   which is using the natural logarithm of the number of subjects for the
#'   penalty parameter `k`.
#' @importFrom stats BIC
#' @exportS3Method
#' @examples
#' # BIC:
#' BIC(object)
BIC.mmrm_tmb <- function(object, ...) {
  # nolint
  k <- log(component(object, "n_subjects"))
  AIC(object, corrected = FALSE, k = k)
}


#' @describeIn mmrm_tmb_methods prints the object.
#' @exportS3Method
print.mmrm_tmb <- function(x,
                           ...) {
  cat("mmrm fit\n\n")

  h_print_call(
    component(x, "call"), component(x, "n_obs"),
    component(x, "n_subjects"), component(x, "n_timepoints")
  )
  h_print_cov(component(x, "cov_type"), component(x, "n_theta"), component(x, "n_groups"))

  cat("Inference:   ")
  cat(ifelse(component(x, "reml"), "REML", "ML"))
  cat("\n")
  cat("Deviance:    ")
  cat(deviance(x))

  cat("\n\nCoefficients: ")
  n_singular_coefs <- sum(component(x, "beta_aliased"))
  if (n_singular_coefs > 0) {
    cat("(", n_singular_coefs, " not defined because of singularities)", sep = "")
  }
  cat("\n")
  print(coef(x, complete = TRUE))

  cat("\nModel Inference Optimization:")

  cat(ifelse(component(x, "convergence") == 0, "\nConverged", "\nFailed to converge"))
  cat(
    " with code", component(x, "convergence"),
    "and message:", tolower(component(x, "conv_message"))
  )
  cat("\n")
  invisible(x)
}


#' @describeIn mmrm_tmb_methods to obtain residuals - either unscaled ('response'), 'pearson' or 'normalized'.
#' @param type (`string`)\cr unscaled (`response`), `pearson` or `normalized`. Default is `response`,
#' and this is the only type available for use with models with a spatial covariance structure.
#' @importFrom stats residuals
#' @exportS3Method
#' @examples
#' # residuals:
#' residuals(object, type = "response")
#' residuals(object, type = "pearson")
#' residuals(object, type = "normalized")
#' @references
#' - \insertRef{galecki2013linear}{mmrm}
residuals.mmrm_tmb <- function(object, type = c("response", "pearson", "normalized"), ...) {
  type <- match.arg(type)
  switch(type,
    "response" = h_residuals_response(object),
    "pearson" = h_residuals_pearson(object),
    "normalized" = h_residuals_normalized(object)
  )
}
#' Calculate Pearson Residuals
#'
#' This is used by [residuals.mmrm_tmb()] to calculate Pearson residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#'
#' @return Vector of residuals.
#'
#' @keywords internal
h_residuals_pearson <- function(object) {
  assert_class(object, "mmrm_tmb")
  h_residuals_response(object) * object$tmb_object$report()$diag_cov_inv_sqrt
}

#' Calculate normalized residuals
#'
#' This is used by [residuals.mmrm_tmb()] to calculate normalized / scaled residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#'
#' @return Vector of residuals
#'
#' @keywords internal
h_residuals_normalized <- function(object) {
  assert_class(object, "mmrm_tmb")
  object$tmb_object$report()$epsilonTilde
}
#' Calculate response residuals.
#'
#' This is used by [residuals.mmrm_tmb()] to calculate response residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#'
#' @return Vector of residuals
#'
#' @keywords internal
h_residuals_response <- function(object) {
  assert_class(object, "mmrm_tmb")
  component(object, "y_vector") - unname(fitted(object))
}
