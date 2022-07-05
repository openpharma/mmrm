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
  object$beta_est
}

#' @describeIn mmrm_tmb_methods obtains the fitted values.
#' @importFrom stats fitted
#' @exportS3Method
#' @examples
#' # Fitted values:
#' fitted(object)
fitted.mmrm_tmb <- function(object, ...) {
  fitted_col <- object$tmb_data$x_matrix %*% object$beta_est
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
  -object$neg_log_lik
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
  object$beta_vcov
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

  x$cov
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
  2 * object$neg_log_lik
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

  n_theta <- length(object$theta_est)
  df <- if (!corrected) {
    n_theta
  } else {
    n_obs <- length(object$tmb_data$y_vector)
    n_beta <- length(object$beta_est)
    m <- max(n_theta + 2, n_obs - n_beta)
    n_theta * (m / (m - n_theta - 1))
  }

  2 * object$neg_log_lik + k * df
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
  k <- log(object$tmb_data$n_subjects)
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
  cat(component(x, "deviance"))

  cat("\n\nCoefficients:\n")
  print(coef(x))

  cat("\nModel Inference Optimization:\n")
  cat("Optimizer: ")

  cat(component(x, "method"))

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
#' @seealso \code{\link[lme4]{getME}}
#' @seealso \code{\link[glmmTMB]{getME}}
#'
#' @export
component <- function(object,
                      name = c(
                        "AIC", "BIC", "logLik", "deviance",
                        "cov_type", "n_theta", "n_subjects", "n_timepoints",
                        "n_obs", "vcov", "varcor", "formula", "dataset",
                        "reml", "method", "convergence", "evaluations",
                        "conv_message", "call"
                      ),
                      ...) {
  assert_class(object, "mmrm_tmb")
  name <- match.arg(name, several.ok = TRUE)

  list_components <- sapply(
    X = name,
    FUN = switch,
    "reml" = object$reml,
    "method" = object$tmb_object$method,
    "convergence" = object$opt_details$convergence,
    "conv_message" = object$opt_details$message,
    "evaluations" = unlist(ifelse(is.null(object$opt_details$evaluations),
      list(object$opt_details$counts),
      list(object$opt_details$evaluations)
    )),
    "call" = object$call,
    "AIC" = AIC(object),
    "BIC" = BIC(object),
    "logLik" = logLik(object),
    "deviance" = deviance(object),
    "logLik" = logLik(object),
    "cov_type" = object$formula_parts$cov_type,
    "n_theta" = length(object$theta_est),
    "n_subjects" = object$tmb_data$n_subjects,
    "n_timepoints" = object$tmb_data$n_visits,
    "n_obs" = length(object$tmb_data$y_vector),
    "vcov" = vcov(object),
    "varcor" = VarCorr(object),
    "formula" = deparse(object$call$formula),
    "dataset" = object$call$data,
    stop(sprintf(
      "component '%s' is not available for class \"%s\"",
      name, class(object)
    ))
  )

  if (length(list_components) == 1) {
    return(list_components[[1]])
    } else return(list_components)

}
