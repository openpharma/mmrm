#' Methods for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM object.
#' @param x (`mmrm_tmb`)\cr same as `object`.
#' @param formula (`mmrm_tmb`)\cr same as `object`.
#' @param complete (`flag`)\cr whether to include potential non-estimable
#'   coefficients.
#' @param ... not used.
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
#' @exportS3Method
#' @examples
#' stop("implement example")
predict.mmrm_tmb <- function(
    object, newdata, se.fit = FALSE, # nolint
    interval = c("none", "confidence", "prediction"), level = 0.95,
    na.action = na.pass, n_sim = 1000L, ...) { # nolint
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
    weights = rep(1, nrow(full_frame)), reml = TRUE,
    singular = "keep", drop_visit_levels = FALSE, allow_na_response = TRUE, drop_levels = FALSE
  )
  predictions <- h_get_prediction(tmb_data, object$theta_est, object$beta_est, object$beta_vcov)
  res <- data.frame(fit = predictions[, 1])
  se <- switch(interval,
    "confidence" = sqrt(predictions[, 2]),
    "prediction" = sqrt(h_get_prediction_variance(object, n_sim, tmb_data)),
    "none" = NULL
  )
  if (se.fit && interval != "none") { # save se
    res <- cbind(res, se = se)
  }
  if (interval != "none") { # compute confidence interval
    alpha <- 1 - level
    z <- qnorm(1 - alpha / 2) * se
    res <- cbind(res,
      lwr = res[, "fit"] - z,
      upr = res[, "fit"] + z
    )
  }
  if (ncol(res) == 1) { # return vector if only fit is computed
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
#' @param n_sim (`integer`)\cr number of replication of sampling.
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
    z <- rnorm(n = n_theta)
    theta_sample <- object$theta_est + theta_chol %*% z
    cond_beta_results <- object$tmb_object$report(theta_sample)
    beta_mean <- cond_beta_results$beta
    beta_cov <- cond_beta_results$beta_vcov
    h_get_prediction(tmb_data, theta_sample, beta_mean, beta_cov)
  })
  mean_of_var <- rowMeans(res[, 1, ])
  var_of_mean <- apply(res[, 3, ], 1, var)
  mean_of_var + var_of_mean
}


#' @describeIn mmrm_tmb_methods obtains the model frame.
#' @param include (`character`)\cr names of variable to include.
#' @param full (`flag`) indicator whether to return full model frame (deprecated).
#' @param na.action (`string`) na action.
#' @importFrom stats model.frame
#' @exportS3Method
#'
#' @details
#' `include` argument controls the variables the returned model frame will include.
#' Possible options are "response_var", "subject_var", "visit_var" and "group_var", representing the
#' response variable, subject variable, visit variable or group variable.
#'
#' @examples
#' # Model frame:
#' model.frame(object)
#' model.frame(object, include = "subject_var")
model.frame.mmrm_tmb <- function(formula, include = NULL, full, na.action = "na.omit", ...) { # nolint
  include_choice <- c("subject_var", "visit_var", "group_var", "response_var")
  if (!missing(full) && identical(full, TRUE)) {
    lifecycle::deprecate_warn("0.3", "model.frame.mmrm_tmb(full)")
    include <- include_choice
  }
  assert_subset(include, include_choice)
  dots <- list(...)
  if (!is.null(dots$subset) || !is.null(dots$weights)) {
    warning("subset and weights are not valid arguments for `mmrm` models.")
  }
  if (is.null(dots$data) && identical(include, include_choice)) {
    formula$tmb_data$full_frame
  } else {
    drop_response <- !"response_var" %in% include
    add_vars <- unlist(formula$formula_parts[include])
    new_formula <- h_add_terms(formula$formula_parts$model_formula, add_vars, drop_response)
    new_data <- h_default_value(dots$data, formula$tmb_data$data)
    full_frame <- formula$tmb_data$full_frame
    for (v in all.vars(formula$formula_parts$model_formula)) {
      if (is.factor(full_frame[[v]]) || is.character(full_frame[[v]])) {
        new_data[[v]] <- h_factor_ref(new_data[[v]], full_frame[[v]])
      }
    }
    model.frame(
      formula = new_formula,
      data = new_data,
      na.action = na.action
    )
  }
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
