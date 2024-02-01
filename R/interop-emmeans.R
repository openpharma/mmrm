#' Support for `emmeans`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This package includes methods that allow `mmrm` objects to be used
#' with the `emmeans` package. `emmeans` computes estimated marginal means
#' (also called least-square means) for the coefficients of the MMRM.
#' We can also e.g. obtain differences between groups by applying
#' [`pairs()`][emmeans::pairs.emmGrid()] on the object returned
#' by [emmeans::emmeans()].
#'
#' @examples
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' if (require(emmeans)) {
#'   emmeans(fit, ~ ARMCD | AVISIT)
#'   pairs(emmeans(fit, ~ ARMCD | AVISIT), reverse = TRUE)
#' }
#' @name emmeans_support
NULL

#' Returns a `data.frame` for `emmeans` Purposes
#'
#' @seealso See [emmeans::recover_data()] for background.
#' @keywords internal
#' @noRd
recover_data.mmrm <- function(object, ...) { # nolint
  fun_call <- stats::getCall(object)
  # subject_var is excluded because it should not contain fixed effect.
  # visit_var is not excluded because emmeans can provide marginal mean
  # by each visit.
  model_frame <- stats::model.frame(object, include = c("visit_var", "response_var", "group_var"))
  model_terms <- stats::delete.response(stats::terms(model_frame))
  emmeans::recover_data(
    fun_call,
    trms = model_terms,
    na.action = "na.omit",
    frame = model_frame,
    ...
  )
}

#' Returns a List of Model Details for `emmeans` Purposes
#'
#' @seealso See [emmeans::emm_basis()] for background.
#' @keywords internal
#' @noRd
emm_basis.mmrm <- function(object, # nolint
                           trms,
                           xlev,
                           grid,
                           ...) {
  model_frame <- stats::model.frame(trms, grid, na.action = stats::na.pass, xlev = xlev)
  contrasts <- attr(component(object, "x_matrix"), "contrasts")
  model_mat <- stats::model.matrix(trms, model_frame, contrasts.arg = contrasts)
  beta_hat <- component(object, "beta_est")
  nbasis <- if (length(beta_hat) < ncol(model_mat)) {
    kept <- match(names(beta_hat), colnames(model_mat))
    beta_hat <- NA * model_mat[1L, ]
    beta_hat[kept] <- component(object, "beta_est")
    orig_model_mat <- stats::model.matrix(
      trms,
      stats::model.frame(object, include = c("visit_var", "response_var", "group_var")),
      contrasts.arg = contrasts
    )
    estimability::nonest.basis(orig_model_mat)
  } else {
    estimability::all.estble
  }
  dfargs <- list(object = object)
  dffun <- function(k, dfargs) {
    mmrm::df_md(dfargs$object, contrast = k)$denom_df
  }
  list(
    X = model_mat,
    bhat = beta_hat,
    nbasis = nbasis,
    V = component(object, "beta_vcov"),
    dffun = dffun,
    dfargs = dfargs
  )
}
