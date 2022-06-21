#' Methods for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM object.
#' @param x (`mmrm_tmb`)\cr same as `object`.
#' @param ... not used.
#'
#' @name mmrm_tmb_methods
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' data <- fev_data
#' object <- mmrm_tmb(formula, data)
NULL

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
