#' Methods for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM object.
#' @param x (`mmrm_tmb`)\cr same as `object`.
#' @param formula (`mmrm_tmb`)\cr same as `object`.
#' @param ... not used.
#'
#' @name mmrm_tmb_methods
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' object <- h_mmrm_tmb(formula, fev_data)
NULL

#' @describeIn mmrm_tmb_methods obtains the estimated coefficients.
#' @importFrom stats coef
#' @exportS3Method
#' @examples
#' # Estimated coefficients:
#' coef(object)
coef.mmrm_tmb <- function(object, ...) {
  component(object, "beta_est")
}

#' @describeIn mmrm_tmb_methods obtains the fitted values.
#' @importFrom stats fitted
#' @exportS3Method
#' @examples
#' # Fitted values:
#' fitted(object)
fitted.mmrm_tmb <- function(object, ...) {
  fitted_col <- component(object, "x_matrix") %*%  component(object, "beta_est")
  fitted_col[, 1L, drop = TRUE]
}

#' @describeIn mmrm_tmb_methods obtains the model frame.
#' @param full (`flag`)\cr whether to include subject and visit variable.
#' @importFrom stats model.frame
#' @exportS3Method
#' @examples
#' # Model frame:
#' model.frame(object)
#' model.frame(object, full = TRUE)
model.frame.mmrm_tmb <- function(formula, full = FALSE, ...) {
  assert_flag(full)
  if (full) {
    formula$tmb_data$full_frame
  } else {
    model.frame(
      formula = formula$formula_parts$model_formula,
      data = formula$tmb_data$full_frame
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
vcov.mmrm_tmb <- function(object, ...) {
  component(object, "vcov")
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
#'   `n_theta + 2` if it is smaller than that.
#' @param corrected (`flag`)\cr whether corrected AIC should be calculated.
#' @param k (`number`)\cr the penalty per parameter to be used; default `k = 2`
#'   is the classical AIC.
#' @importFrom stats AIC
#' @exportS3Method
#' @examples
#' # AIC:
#' AIC(object)
#' AIC(object, corrected = TRUE)
AIC.mmrm_tmb <- function(object, corrected = FALSE, ..., k = 2) { # nolint
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
BIC.mmrm_tmb <- function(object, ...) { # nolint
  k <- log(component(object, "n_subjects"))
  AIC(object, corrected = FALSE, k = k)
}


#' @describeIn mmrm_tmb_methods prints the object.
#' @exportS3Method
#' @keywords internal
print.mmrm_tmb <- function(x,
                           ...) {
  cat("mmrm fit\n\n")

  h_print_call(
    component(x, "call"), component(x, "n_obs"),
    component(x, "n_subjects"), component(x, "n_timepoints")
  )
  h_print_cov(component(x, "cov_type"), component(x, "n_theta"))

  cat("Method: ")
  cat(ifelse(component(x, "reml"), "REML", "ML"))
  cat("\nDeviance: ")
  cat(deviance(x))

  cat("\n\nCoefficients:\n")
  print(coef(x))

  cat("\nModel Inference Optimization:")

  cat(ifelse(component(x, "convergence") == 0, "\nConverged", "\nFailed to converge"))
  cat(
    " with code", component(x, "convergence"),
    "and message:", tolower(component(x, "conv_message"))
  )

  invisible(x)
}

#' @describeIn mmrm_tmb_methods get components from the \code{mmrm_tmb} object
#'
#' @aliases component
#' @param x a \code{mmrm_tmb} object
#' @param name of the component to be retrieved
#' @param \dots ignored, for method compatibility
#'
#' @details Available `component()` names are as follows:
#' - 'call': low-level function call which generated the model.
#' - 'formula': model formula.
#' - 'dataset': data set name.
#' - 'cov_type': covariance structure type.
#' - 'n_theta': number of parameters.
#' - 'n_subjects': number of subjects.
#' - 'n_timepoints': number of modeled time points.
#' - 'n_obs': total number of observations.
#' - 'reml': was REML used (ML was used if \code{FALSE}).
#' - 'neg_log_lik': negative log likelihood.
#' - 'convergence': convergence code from optimizer.
#' - 'conv_message': message accompanying the convergence code.
#' - 'evaluations': number of function evaluations for optimization.
#' - 'vcov': estimated variance-covariance matrix of coefficients.
#' - 'varcor': estimated covariance matrix for residuals.
#' - 'theta_est': estimated variance parameters.
#' - 'beta_est': estimated coefficients.
#' - 'theta_vcov':  estimated variance-covariance matrix of variance parameters.
#' - 'x_matrix': design matrix used.
#' - 'y_vector': response vector used.
#' - 'jac_list': Jacobian, see  [h_jac_list()] for details.
#'
#' @seealso \code{\link[lme4]{getME}}
#' @seealso \code{\link[glmmTMB]{getME}}
#'
#' @export
component <- function(object,
                      name = c(
                        "cov_type", "n_theta", "n_subjects", "n_timepoints",
                        "n_obs", "vcov", "varcor", "formula", "dataset",
                        "reml", "convergence", "evaluations",
                        "conv_message", "call", "theta_est",
                        "beta_est", "x_matrix", "y_vector", "neg_log_lik",
                        "jac_list", "theta_vcov"
                      ),
                      ...) {
  assert_class(object, "mmrm_tmb")
  name <- match.arg(name, several.ok = TRUE)


  list_components <- sapply(
    X = name,
    FUN = switch,
    "call" = object$call,
    # Strings
    "cov_type" = object$formula_parts$cov_type,
    "formula" = deparse(object$call$formula),
    "dataset" = object$call$data,
    "reml" = object$reml,
    "conv_message" = object$opt_details$message,
    # Numeric of length 1
    "convergence" = object$opt_details$convergence,
    "neg_log_lik" = object$neg_log_lik,
    "n_theta" = length(object$theta_est),
    "n_subjects" = object$tmb_data$n_subjects,
    "n_timepoints" = object$tmb_data$n_visits,
    "n_obs" = length(object$tmb_data$y_vector),
    # Numeric of length > 1
    "evaluations" = unlist(ifelse(is.null(object$opt_details$evaluations),
      list(object$opt_details$counts),
      list(object$opt_details$evaluations)
    )),
    "beta_est" = object$beta_est,
    "theta_est" = object$theta_est,
    "y_vector" = object$tmb_data$y_vector,
    "jac_list" = object$jac_list,
    # Matrices
    "vcov" = object$beta_vcov,
    "varcor" = object$cov,
    "x_matrix" = object$tmb_data$x_matrix,
    "theta_vcov" = object$theta_vcov,
    # If not found
    "..foo.." =
      stop(sprintf(
        "component '%s' is not available for class \"%s\"",
        name, paste0(class(object), collapse = ", ")
      )), simplify = FALSE
  )

  if (length(name) == 1) list_components[[1]] else list_components
}

