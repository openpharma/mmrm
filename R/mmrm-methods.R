#' Methods for `mmrm` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm`)\cr the fitted MMRM including Jacobian and call etc.
#' @param ... not used.
#'
#' @name mmrm_methods
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' object <- mmrm(formula, fev_data)
NULL

#' Coefficients Table for MMRM Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is used by [summary.mmrm()] to obtain the coefficients table.
#'
#' @param object (`mmrm`)\cr model fit.
#'
#' @return Matrix with one row per coefficient and columns
#'   `Estimate`, `Std. Error`, `df`, `t value` and `Pr(>|t|)`.
#' @export
h_coef_table <- function(object) {
  assert_class(object, "mmrm")

  coef_est <- coef(object)
  coef_contrasts <- diag(x = rep(1, length(coef_est)))
  rownames(coef_contrasts) <- names(coef_est)
  coef_table <- t(apply(
    coef_contrasts,
    MARGIN = 1L,
    FUN = \(contrast) unlist(df_1d(object, contrast))
  ))
  assert_names(
    colnames(coef_table),
    identical.to = c("est", "se", "df", "t_stat", "p_val")
  )
  colnames(coef_table) <- c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")
  coef_table
}

#' @describeIn mmrm_methods summarizes the MMRM fit results.
#' @exportS3Method
#' @examples
#' # Summary:
#' summary(object)
summary.mmrm <- function(object, ...) {
  aic_list <- list(
    AIC = AIC(object),
    BIC = BIC(object),
    logLik = logLik(object),
    deviance = deviance(object)
  )
  structure(
    list(
      logLik = logLik(object),
      n_subjects = object$tmb_data$n_subjects,
      n_visits = object$tmb_data$n_visits,
      n_obs = length(object$tmb_data$y_vector),
      coefficients = h_coef_table(object),
      vcov = vcov(object),
      varcor = VarCorr(object),
      aic_list = aic_list,
      call = stats::getCall(object)
    ),
    class = "summary.mmrm"
  )
}

#' Printing MMRM Function Call
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is used in [print.summary.mmrm()].
#'
#' @param call (`call`)\cr original [mmrm()] function call.
#'
#' @export
h_print_call <- function(call) {
  pass <- 0
  if (!is.null(tmp <- call$formula)) {
    cat("Formula: ", deparse(tmp), fill = TRUE)
    rhs <- tmp[[2]]
    pass <- nchar(deparse(rhs))
  }
  if (!is.null(tmp <- call$data)) {
    cat("Data:    ", deparse(tmp), fill = TRUE)
  }
}

#' Printing AIC and other Model Fit Criteria
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is used in [print.summary.mmrm()].
#'
#' @param aic_list (`list`)\cr list as part of from [summary.mmrm()].
#' @param digits (`number`)\cr number of decimal places used with [round()].
#'
#' @export
h_print_aic_list <- function(aic_list,
                             digits = 1) {
  diag_vals <- round(unlist(aic_list), digits)
  diag_vals <- format(diag_vals)
  print(diag_vals, quote = FALSE)
}

#' @describeIn mmrm_methods prints the MMRM fit summary.
#' @exportS3Method
#' @keywords internal
print.summary.mmrm <- function(x,
                               digits = max(3, getOption("digits") - 3),
                               signif.stars = getOption("show.signif.stars"),
                               ...) {
  cat("mmrm fit\n\n")
  h_print_call(x$call)
  cat(
    "Used", x$n_obs, "observations from", x$n_subjects,
    "subjects with maximum", x$n_visits, "timepoints.\n"
  )
  cat("\n")
  cat("Model selection criteria:\n")
  h_print_aic_list(x$aic_list)
  cat("\n")
  cat("Coefficients:\n")
  stats::printCoefmat(
    x$coefficients,
    zap.ind = 3,
    digits = digits,
    signif.stars = signif.stars
  )
  cat("\n")
  cat("Covariance estimate:\n")
  print(round(x$varcor, digits = digits))
  cat("\n")
  invisible(x)
}
