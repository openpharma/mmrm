#' Methods for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM object.
#' @param x (`mmrm_tmb`)\cr same as `object`.
#' @param formula (`mmrm_tmb`)\cr same as `object`.
#' @param complete (`flag`)\cr whether to include potential non-estimable
#'   coefficients.
#' @param ... mostly not used;
#'   Exception is `model.matrix()` passing `...` to the default method.
#' @return Depends on the method, see Functions.
#'
#' @name mmrm_tmb_methods
#'
#' @seealso [`mmrm_methods`], [`mmrm_tidiers`] for additional methods.
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
#'
#' @note If the response in the formula is a complicated expression, e.g. with multiple
#'   variables, and not all variables are provided in `newdata`, a warning is issued.
#'   In this case it is recommended to provide all variables in the `newdata` data set,
#'   and manually set the response variables to `NA` as needed for obtaining unconditional
#'   predictions e.g.
#'
#' @param newdata (`data.frame`)\cr optional new data, otherwise data from `object` is used.
#' @param se.fit (`flag`)\cr indicator if standard errors are required.
#' @param interval (`string`)\cr type of interval calculation. Can be abbreviated.
#' @param level (`number`)\cr tolerance/confidence level.
#' @param nsim (`count`)\cr number of simulations to use.
#' @param conditional (`flag`)\cr indicator if the prediction is conditional on the observation or not.
#'
#' @importFrom stats predict
#' @exportS3Method
#'
#' @examples
#' predict(object, newdata = fev_data)
predict.mmrm_tmb <- function(
  object,
  newdata,
  se.fit = FALSE, # nolint
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  nsim = 1000L,
  conditional = FALSE,
  ...
) {
  if (missing(newdata)) {
    newdata <- object$data
  }
  assert_data_frame(newdata)
  orig_row_names <- row.names(newdata)
  assert_flag(se.fit)
  assert_number(level, lower = 0, upper = 1)
  assert_count(nsim, positive = TRUE)
  assert_flag(conditional)
  interval <- match.arg(interval)
  formula_parts <- object$formula_parts
  # Check if the response is a simple variable name vs a complex expression
  is_simple_response_var <- is(object$formula_parts$formula[[2]], "name")
  missing_resp_vars <- setdiff(formula_parts$response_var, names(newdata))
  has_missing_resp_vars <- length(missing_resp_vars) > 0
  if (!is_simple_response_var && has_missing_resp_vars) {
    warning(
      "complicated response expression in formula and not all variables in left-hand side are provided, ",
      "using NA to replace the missing variables; ",
      "this could potentially lead to unexpected behavior."
    )
  }
  # Make sure we have at least completely NA filled response variables in newdata.
  for (var in missing_resp_vars) {
    newdata[[var]] <- NA_real_
  }
  if (any(object$tmb_data$x_cols_aliased)) {
    warning(
      "In fitted object there are co-linear variables and therefore dropped terms, ",
      "and this could lead to incorrect prediction on new data."
    )
  }
  colnames <- names(Filter(isFALSE, object$tmb_data$x_cols_aliased))
  if (!conditional && interval %in% c("none", "confidence")) {
    # model.matrix always return a complete matrix (no NA allowed)
    x_mat <- stats::model.matrix(object, data = newdata, use_response = FALSE)[,
      colnames,
      drop = FALSE
    ]
    x_mat_full <- matrix(
      NA,
      nrow = nrow(newdata),
      ncol = ncol(x_mat),
      dimnames = list(row.names(newdata), colnames(x_mat))
    )
    x_mat_full[row.names(x_mat), ] <- x_mat
    predictions <- (x_mat_full %*% component(object, "beta_est"))[, 1]
    predictions_raw <- stats::setNames(
      rep(NA_real_, nrow(newdata)),
      row.names(newdata)
    )
    predictions_raw[names(predictions)] <- predictions
    if (identical(interval, "none")) {
      return(predictions_raw)
    }
    se <- switch(
      interval,
      # can be NA if there are aliased cols
      "confidence" = diag(
        x_mat_full %*% component(object, "beta_vcov") %*% t(x_mat_full)
      ),
      "none" = NA_real_
    )
    res <- cbind(
      fit = predictions,
      se = se,
      lwr = predictions - stats::qnorm(1 - level / 2) * se,
      upr = predictions + stats::qnorm(1 - level / 2) * se
    )
    if (!se.fit) {
      res <- res[, setdiff(colnames(res), "se")]
    }
    res_raw <- matrix(
      NA_real_,
      ncol = ncol(res),
      nrow = nrow(newdata),
      dimnames = list(row.names(newdata), colnames(res))
    )
    res_raw[row.names(res), ] <- res
    return(res_raw)
  }
  tmb_data <- h_mmrm_tmb_data(
    formula_parts,
    newdata,
    weights = rep(1, nrow(newdata)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE,
    xlev = component(object, "xlev"),
    contrasts = component(object, "contrasts")
  )
  tmb_data$x_matrix <- tmb_data$x_matrix[, colnames, drop = FALSE]
  predictions <- h_get_prediction(
    tmb_data,
    object$theta_est,
    object$beta_est,
    component(object, "beta_vcov")
  )$prediction
  res <- cbind(fit = rep(NA_real_, nrow(newdata)))
  new_order <- match(row.names(tmb_data$full_frame), orig_row_names)
  res[new_order, "fit"] <- predictions[, "fit"]
  se <- switch(
    interval,
    "confidence" = sqrt(predictions[, "conf_var"]),
    "prediction" = sqrt(h_get_prediction_variance(object, nsim, tmb_data)),
    "none" = NULL
  )
  if (interval != "none") {
    res <- cbind(
      res,
      se = NA_real_
    )
    res[new_order, "se"] <- se
    alpha <- 1 - level
    z <- stats::qnorm(1 - alpha / 2) * res[, "se"]
    res <- cbind(
      res,
      lwr = res[, "fit"] - z,
      upr = res[, "fit"] + z
    )
    if (!se.fit) {
      res <- res[, setdiff(colnames(res), "se")]
    }
  }
  # Use original names.
  row.names(res) <- orig_row_names
  if (ncol(res) == 1) {
    res <- res[, "fit"]
  }
  return(res)
}

#' Get Prediction
#'
#' @description Get predictions with given `data`, `theta`, `beta`, `beta_vcov`.
#'
#' @details See `predict` function in `predict.cpp` which is called internally.
#'
#' @param tmb_data (`mmrm_tmb_data`)\cr object.
#' @param theta (`numeric`)\cr theta value.
#' @param beta (`numeric`)\cr beta value.
#' @param beta_vcov (`matrix`)\cr beta_vcov matrix.
#'
#' @return List with:
#' - `prediction`: Matrix with columns `fit`, `conf_var`, and `var`.
#' - `covariance`: List with subject specific covariance matrices.
#' - `index`: List of zero-based subject indices.
#'
#' @keywords internal
h_get_prediction <- function(tmb_data, theta, beta, beta_vcov) {
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_numeric(theta)
  n_beta <- ncol(tmb_data$x_matrix)
  assert_numeric(beta, finite = TRUE, any.missing = FALSE, len = n_beta)
  assert_matrix(
    beta_vcov,
    mode = "numeric",
    any.missing = FALSE,
    nrows = n_beta,
    ncols = n_beta
  )
  .Call(`_mmrm_predict`, PACKAGE = "mmrm", tmb_data, theta, beta, beta_vcov)
}

#' Get Prediction Variance
#'
#' @description Get prediction variance with given fit, `tmb_data` with the Monte Carlo sampling method.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#' @param nsim (`count`)\cr number of samples.
#' @param tmb_data (`mmrm_tmb_data`)\cr object.
#'
#' @keywords internal
h_get_prediction_variance <- function(object, nsim, tmb_data) {
  assert_class(object, "mmrm_tmb")
  assert_class(tmb_data, "mmrm_tmb_data")
  assert_count(nsim, positive = TRUE)
  theta_chol <- chol(eval(object$theta_vcov))
  n_theta <- length(object$theta_est)
  res <- replicate(nsim, {
    z <- stats::rnorm(n = n_theta)
    theta_sample <- object$theta_est + z %*% theta_chol
    cond_beta_results <- object$tmb_object$report(theta_sample)
    beta_mean <- cond_beta_results$beta
    beta_cov <- cond_beta_results$beta_vcov
    h_get_prediction(tmb_data, theta_sample, beta_mean, beta_cov)$prediction
  })
  mean_of_var <- rowMeans(res[, "var", ])
  var_of_mean <- apply(res[, "fit", ], 1, stats::var)
  mean_of_var + var_of_mean
}

#' @describeIn mmrm_tmb_methods obtains the model frame.
#' @param data (`data.frame`)\cr object in which to construct the frame.
#' @param include (`character`)\cr names of variable types to include in the model frame creation.
#'   Must be `NULL` or one or more of `c("subject_var", "visit_var", "group_var", "response_var")`.
#' @param exclude (`character`)\cr names of variable types to exclude after the model frame creation.
#'   Same choices as for `include`.
#' @param full (`flag`)\cr indicator whether to return full model frame (deprecated).
#' @param na.action (`string`)\cr na action.
#' @importFrom stats model.frame
#' @exportS3Method
#'
#' @details
#' `include` argument controls the variables the model frame will include upon creation.
#' Possible options are "response_var", "subject_var", "visit_var" and "group_var", representing the
#' response variable, subject variable, visit variable or group variable.
#' By default all four variable types are included, which means that the original model frame will be used.
#' If only a subset of variable types is requested, the model frame will be constructed freshly, and
#' if there are more complete rows (without `NA`s) in the required variables than in the original model frame,
#' those rows will be included as well.
#'
#' The `exclude` argument can be used to exclude specific variable types after the model frame creation.
#' By default no variable types are excluded. The `exclude` argument is useful when the original
#' model frame should be used as basis (with same incomplete rows omitted), but some variable types should be removed
#' afterwards.
#'
#' `character` values in new data will always be factorized according to the data in the fit
#' to avoid mismatched in levels or issues in `model.matrix`.
#'
#' @examples
#' # Model frame:
#' model.frame(object)
#' model.frame(object, include = "subject_var")
model.frame.mmrm_tmb <- function(
  formula,
  data,
  include = c("subject_var", "visit_var", "group_var", "response_var"),
  exclude = NULL,
  full,
  na.action = "na.omit", # nolint
  ...
) {
  # nolint
  # Construct updated formula and data arguments.
  lst_formula_and_data <-
    h_construct_model_frame_inputs(
      formula = formula,
      data = data,
      include = include,
      full = full
    )
  # Only if include is default (full) and also data is missing, and also na.action is na.omit we will
  # use the model frame from the tmb_data.
  include_choice <- c("subject_var", "visit_var", "group_var", "response_var")
  if (
    missing(data) &&
      setequal(include, include_choice) &&
      identical(h_get_na_action(na.action), stats::na.omit)
  ) {
    ret <- formula$tmb_data$full_frame
    # Remove weights column.
    ret[, "(weights)"] <- NULL
    ret
  } else {
    # Construct data frame.
    ret <-
      stats::model.frame(
        formula = lst_formula_and_data$formula,
        data = lst_formula_and_data$data,
        na.action = na.action,
        xlev = component(formula, "xlev")
      )
  }
  # Exclude requested variable types.
  if (!is.null(exclude)) {
    assert_subset(
      exclude,
      c("subject_var", "visit_var", "group_var", "response_var")
    )
    vars_to_exclude <- unlist(formula$formula_parts[exclude])
    ret <- ret[, setdiff(colnames(ret), vars_to_exclude), drop = FALSE]
  }
  ret
}


#' Construction of Model Frame Formula and Data Inputs
#'
#' @description
#' Input formulas are converted from `mmrm`-style to a style compatible
#' with default [stats::model.frame()] and [stats::model.matrix()] methods.
#'
#' The full formula is returned so we can construct, for example, the
#' `model.frame()` including all columns as well as the requested subset.
#' The full set is used to identify rows to include in the reduced model frame.
#'
#' @param formula (`mmrm`)\cr fit object.
#' @param data (`data.frame`)\cr optional data frame will be
#'   passed to `model.frame()` or `model.matrix()`.
#' @param include (`character`)\cr specification of variables to include.
#' @param full (`flag`)\cr indicator whether to return full model frame (deprecated).
#'
#' @return A named list with two elements:
#'
#' - `"formula"`: the formula including the columns requested in the `include=` argument.
#' - `"data"`: a data frame including all columns needed in the formula.
#'
#' @keywords internal
h_construct_model_frame_inputs <- function(
  formula,
  data,
  include,
  include_choice = c("subject_var", "visit_var", "group_var", "response_var"),
  full
) {
  if (!missing(full) && identical(full, TRUE)) {
    lifecycle::deprecate_warn("0.3", "model.frame.mmrm_tmb(full)")
    include <- include_choice
  }

  assert_class(formula, classes = "mmrm_tmb")
  assert_subset(include, include_choice)
  if (missing(data)) {
    data <- formula$data
  }
  assert_data_frame(data)

  drop_response <- !"response_var" %in% include
  add_vars <- unlist(formula$formula_parts[setdiff(include, "response_var")])
  new_formula <- h_add_terms(
    formula$formula_parts$model_formula,
    add_vars,
    drop_response
  )

  drop_response_full <- !"response_var" %in% include_choice
  add_vars_full <- unlist(formula$formula_parts[include_choice])
  new_formula_full <-
    h_add_terms(
      formula$formula_parts$model_formula,
      add_vars_full,
      drop_response_full
    )

  # Update data based on the columns in the full formula return.
  all_vars <- all.vars(new_formula_full)
  all_vars <- intersect(colnames(data), all_vars) # Because some variables could be from the environment.
  data <- data[, all_vars, drop = FALSE]

  # Return list with updated formula, data.
  list(
    formula = new_formula,
    data = data
  )
}

#' @describeIn mmrm_tmb_methods obtains the model matrix.
#' @exportS3Method
#' @param use_response (`flag`)\cr whether to use the response for complete rows.
#' @param contrasts.arg (`list`)\cr contrasts to be used.
#'
#' @examples
#' # Model matrix:
#' model.matrix(object)
model.matrix.mmrm_tmb <- function(
  object,
  data,
  use_response = TRUE,
  # nolint start
  contrasts.arg = attr(object$tmb_data$x_matrix, "contrasts"),
  # nolint end
  ...
) {
  # Always return the utilized model matrix if data not provided.
  if (missing(data)) {
    return(object$tmb_data$x_matrix)
  }
  stats::model.matrix(
    h_add_terms(
      object$formula_parts$model_formula,
      NULL,
      drop_response = !use_response
    ),
    data = data,
    contrasts.arg = contrasts.arg,
    xlev = component(object, "xlev"),
    ...
  )
}

#' @describeIn mmrm_tmb_methods obtains the terms object.
#' @importFrom stats model.frame
#' @exportS3Method
#'
#' @examples
#' # terms:
#' terms(object)
#' terms(object, include = "subject_var")
terms.mmrm_tmb <- function(x, include = "response_var", ...) {
  # nolint
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
#'   Includes as attributes the number of variance parameters `n_param`, number
#'   of estimated coefficients `n_coef`, degrees of freedom `df`, and number of
#'   subjects `nobs`. The `nobs` attribute is so named so that if this
#'   function's results are passed to `stats::BIC()`, the BIC value will be
#'   calculated correctly. Resulting value is of class
#'   [`logLik`][stats::logLik].
#' @importFrom stats logLik
#' @exportS3Method
#' @examples
#' # Log likelihood given the estimated parameters:
#' logLik(object)
logLik.mmrm_tmb <- function(object, ...) {
  out <- -component(object, "neg_log_lik")

  # Number of variance parameters.
  n_param <- component(object, "n_theta")

  # Number of estimated coefficients.
  n_coef <- length(coef(object, complete = FALSE))

  # Number of degrees of freedom. Note that the number of coefficients is added
  # only if the fit was estimated using ML rather than REML.
  df <- n_param + n_coef * !component(object, "reml")

  n_subjects <- component(object, "n_subjects")

  structure(
    out,
    n_param = n_param,
    n_coef = n_coef,
    df = df,
    nobs = n_subjects,
    class = "logLik"
  )
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
VarCorr.mmrm_tmb <- function(x, sigma = NA, ...) {
  # nolint
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
print.mmrm_tmb <- function(x, ...) {
  cat("mmrm fit\n\n")

  h_print_call(
    component(x, "call"),
    component(x, "n_obs"),
    component(x, "n_subjects"),
    component(x, "n_timepoints")
  )
  h_print_cov(
    component(x, "cov_type"),
    component(x, "n_theta"),
    component(x, "n_groups")
  )

  cat("Inference:   ")
  cat(ifelse(component(x, "reml"), "REML", "ML"))
  cat("\n")
  cat("Deviance:    ")
  cat(deviance(x))

  cat("\n\nCoefficients: ")
  n_singular_coefs <- sum(component(x, "beta_aliased"))
  if (n_singular_coefs > 0) {
    cat(
      "(",
      n_singular_coefs,
      " not defined because of singularities)",
      sep = ""
    )
  }
  cat("\n")
  print(coef(x, complete = TRUE))

  cat("\nModel Inference Optimization:")

  cat(ifelse(
    component(x, "convergence") == 0,
    "\nConverged",
    "\nFailed to converge"
  ))
  cat(
    " with code",
    component(x, "convergence"),
    "and message:",
    if (is.null(component(x, "conv_message"))) {
      "No message provided."
    } else {
      tolower(component(x, "conv_message"))
    }
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
residuals.mmrm_tmb <- function(
  object,
  type = c("response", "pearson", "normalized"),
  ...
) {
  type <- match.arg(type)
  switch(
    type,
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

#' @describeIn mmrm_tmb_methods simulate responses from a fitted model according
#'   to the simulation `method`, returning a `data.frame` of dimension `[n, m]`
#'   where n is the number of rows in `newdata`,
#'   and m is the number `nsim` of simulated responses.
#'
#' @param seed unused argument from [stats::simulate()].
#' @param method (`string`)\cr simulation method to use. If "conditional",
#'   simulated values are sampled given the estimated covariance matrix of `object`.
#'   If "marginal", the variance of the estimated covariance matrix is taken into account.
#'
#' @importFrom stats simulate
#' @exportS3Method
simulate.mmrm_tmb <- function(
  object,
  nsim = 1,
  seed = NULL,
  newdata,
  ...,
  method = c("conditional", "marginal")
) {
  assert_count(nsim, positive = TRUE)
  assert_null(seed)
  if (missing(newdata)) {
    newdata <- object$data
  }
  assert_data_frame(newdata)
  method <- match.arg(method)

  tmb_data <- h_mmrm_tmb_data(
    object$formula_parts,
    newdata,
    weights = rep(1, nrow(newdata)),
    reml = TRUE,
    singular = "keep",
    drop_visit_levels = FALSE,
    allow_na_response = TRUE,
    drop_levels = FALSE,
    xlev = component(object, "xlev"),
    contrasts = component(object, "contrasts")
  )
  ret <- if (method == "conditional") {
    predict_res <- h_get_prediction(
      tmb_data,
      object$theta_est,
      object$beta_est,
      object$beta_vcov
    )
    as.data.frame(h_get_sim_per_subj(predict_res, tmb_data$n_subjects, nsim))
  } else if (method == "marginal") {
    theta_chol <- t(chol(object$theta_vcov))
    n_theta <- length(object$theta_est)
    as.data.frame(
      sapply(seq_len(nsim), function(x) {
        newtheta <- object$theta_est +
          theta_chol %*% matrix(stats::rnorm(n_theta), ncol = 1)
        # Recalculate betas with sampled thetas.
        hold <- object$tmb_object$report(newtheta)
        # Resample betas given new beta distribution.
        # We first solve L^\top w = D^{-1/2}z_{sample}:
        w_sample <- backsolve(
          r = hold$XtWX_L,
          x = stats::rnorm(length(hold$beta)) / sqrt(hold$XtWX_D),
          upper.tri = FALSE,
          transpose = TRUE
        )
        # Then we add the mean vector, the beta estimate.
        beta_sample <- hold$beta + w_sample
        predict_res <- h_get_prediction(
          tmb_data,
          newtheta,
          beta_sample,
          hold$beta_vcov
        )
        h_get_sim_per_subj(predict_res, tmb_data$n_subjects, 1L)
      })
    )
  }
  orig_row_names <- row.names(newdata)
  new_order <- match(orig_row_names, row.names(tmb_data$full_frame))
  ret[new_order, , drop = FALSE]
}

#' Get simulated values by patient.
#'
#' @param predict_res (`list`)\cr from [h_get_prediction()].
#' @param nsub (`count`)\cr number of subjects.
#' @param nsim (`count`)\cr number of values to simulate.
#'
#' @keywords internal
h_get_sim_per_subj <- function(predict_res, nsub, nsim) {
  assert_list(predict_res)
  assert_count(nsub, positive = TRUE)
  assert_count(nsim, positive = TRUE)

  ret <- matrix(
    predict_res$prediction[, "fit"],
    ncol = nsim,
    nrow = nrow(predict_res$prediction)
  )
  for (i in seq_len(nsub)) {
    # Skip subjects which are not included in predict_res.
    if (length(predict_res$index[[i]]) > 0) {
      # Obtain indices of data.frame belonging to subject i
      # (increment by 1, since indices from cpp are 0-order).
      inds <- predict_res$index[[i]] + 1
      obs <- length(inds)

      # Get relevant covariance matrix for subject i.
      covmat_i <- predict_res$covariance[[i]]
      theta_chol <- t(chol(covmat_i))

      # Simulate epsilon from covariance matrix.
      mus <- ret[inds, , drop = FALSE]
      epsilons <- theta_chol %*% matrix(stats::rnorm(nsim * obs), ncol = nsim)
      ret[inds, ] <- mus + epsilons
    }
  }

  ret
}
