#' Dynamic Registration of `emmeans` Methods
#'
#' @seealso See `vignette("xtending", package = "emmeans")` for background.
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("emmeans", quietly = TRUE)) {
    if (utils::packageVersion("emmeans") < "1.7") {
      warning("please install a newer version of emmeans (>= 1.7)")
      return(NULL)
    }
    emmeans::.emm_register("mmrm", pkgname)
  }
}

#' Support for `emmeans`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This package includes methods that allow `mmrm` objects to be used
#' with the `emmeans` package. `emmeans` computed estimated marginal means
#' (also called least-square means) for the coefficients of the MMRM.
#'
#' @examples
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' if (require(emmeans)) {
#'   emmeans(fit, ~ ARMCD | AVISIT)
#' }
#' @name emmeans_support
NULL

#' Returns a `data.frame` for `emmeans` Purposes
#'
#' @seealso See [emmeans::recover_data()] for background.
#' @keywords internal
#' @noRd
recover_data.mmrm <- function(object, ...) {
  fun_call <- object$call
  model_frame <- droplevels(stats::model.frame(object$formula_parts$model_formula, data = object$data))
  model_terms <- stats::delete.response(stats::terms(model_frame))
  na_action <- attr(model_frame, "na.action")
  emmeans::recover_data(
    fun_call,
    trms = model_terms,
    na.action = na_action,
    frame = model_frame,
    ...
  )
}

#' Returns a List of Model Details for `emmeans` Purposes
#'
#' @seealso See [emmeans::emm_basis()] for background.
#' @keywords internal
#' @noRd
emm_basis.mmrm <- function(object, trms, xlev, grid, ...) {

}
