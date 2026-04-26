#' Support for `emmeans`
#'
#' @description `r lifecycle::badge("stable")`
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

#' Match coefficient names against model matrix columns
#'
#' This helper is robust against re-ordering of interaction terms, e.g.
#' `A:B` vs `B:A` and `A:B:C` vs any permutation.
#'
#' @keywords internal
#' @noRd
h_match_coefs <- function(coef_names, model_colnames) {
  canon_interaction <- function(x) {
    split_terms <- strsplit(x, ":", fixed = TRUE)
    vapply(
      split_terms,
      FUN.VALUE = character(1),
      function(term_parts) {
        if (length(term_parts) <= 1L) {
          term_parts
        } else {
          paste(sort(term_parts), collapse = ":")
        }
      }
    )
  }

  kept <- match(coef_names, model_colnames)
  idx_na <- is.na(kept)

  if (any(idx_na)) {
    kept[idx_na] <- match(
      canon_interaction(coef_names[idx_na]),
      canon_interaction(model_colnames)
    )
  }

  kept
}

#' Returns a `data.frame` for `emmeans` Purposes
#'
#' @seealso See [emmeans::recover_data()] for background.
#' @keywords internal
#' @noRd
# nolint start
recover_data.mmrm <- function(object, ...) {
  # nolint end
  fun_call <- stats::getCall(object)
  # subject_var is excluded because it should not contain fixed effect.
  # visit_var is only excluded for spatial covariance structures -
  # for non-spatial covariance structures, emmeans can provide marginal mean
  # by each visit so we need visit_var.
  model_frame <- stats::model.frame(
    object,
    exclude = c(
      "subject_var",
      if (object$formula_parts$is_spatial) "visit_var"
    )
  )
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
# nolint start
emm_basis.mmrm <- function(
  # nolint end
  object,
  trms,
  xlev,
  grid,
  ...
) {
  model_frame <- stats::model.frame(
    trms,
    grid,
    na.action = stats::na.pass,
    xlev = xlev
  )
  contrasts <- component(object, "contrasts")
  model_mat <- stats::model.matrix(trms, model_frame, contrasts.arg = contrasts)
  beta_hat <- component(object, "beta_est")
  nbasis <- if (length(beta_hat) < ncol(model_mat)) {
    kept <- h_match_coefs(names(beta_hat), colnames(model_mat))
    if (anyNA(kept)) {
      unmatched <- names(beta_hat)[is.na(kept)]
      stop(
        paste0(
          "Failed to match coefficient name(s) to model matrix columns: ",
          paste(unmatched, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    beta_hat <- NA * model_mat[1L, ]
    beta_hat[kept] <- component(object, "beta_est")
    orig_model_mat <- stats::model.matrix(
      trms,
      stats::model.frame(
        object,
        exclude = c(
          "subject_var",
          if (object$formula_parts$is_spatial) "visit_var"
        )
      ),
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
