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

  # Start with the model-based covariance.
  V <- component(object, "beta_vcov")

  if (!is.null(object$emmeans_gcomp_vars)) {
    gcomp_result <- h_emm_basis_gcomp(
      object,
      model_mat,
      beta_hat,
      nbasis,
      V,
      grid
    )
    model_mat <- gcomp_result$model_mat
    beta_hat <- gcomp_result$beta_hat
    nbasis <- gcomp_result$nbasis
    V <- gcomp_result$V
    dfargs <- gcomp_result$dfargs
    dffun <- gcomp_result$dffun
    message(
      "G-computation correction applied, so emmeans will produce the correct average treatment effect."
    )
  } else {
    dfargs <- list(object = object)
    dffun <- function(k, dfargs) {
      mmrm::df_md(dfargs$object, contrast = k)$denom_df
    }
  }

  list(
    X = model_mat,
    bhat = beta_hat,
    nbasis = nbasis,
    V = V,
    dffun = dffun,
    dfargs = dfargs
  )
}

#' Apply G-computation Correction in emm_basis
#'
#' @description Applies the G-computation correction to the emmeans basis
#'   components. Replaces the L matrix with globally-weighted rows, adds
#'   the variance correction to V, and handles aliased coefficient expansion.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#' @param model_mat (`matrix`)\cr the L matrix from emmeans.
#' @param beta_hat (`numeric`)\cr coefficient estimates (may contain NA for
#'   aliased positions).
#' @param nbasis (`matrix` or `NULL`)\cr estimability basis from emmeans.
#' @param V (`matrix`)\cr model-based covariance of beta.
#' @param grid (`data.frame`)\cr the emmeans reference grid.
#'
#' @return A list with modified `model_mat`, `beta_hat`, `nbasis`, `V`,
#'   `dfargs`, and `dffun`.
#'
#' @keywords internal
#' @noRd
h_emm_basis_gcomp <- function(object, model_mat, beta_hat, nbasis, V, grid) {
  # Detect aliased coefficient positions before any modifications.
  aliased_pos <- which(is.na(beta_hat))

  visit_var <- object$formula_parts$visit_var
  V_corrected_est <- V

  if (visit_var %in% names(grid)) {
    # Save original model_mat dimensions. When aliased coefficients exist,
    # h_match_coefs expands model_mat beyond length(beta_est). The correction
    # works in estimable-coefficient space, then we re-expand to match.
    orig_ncol <- ncol(model_mat)
    orig_colnames <- colnames(model_mat)

    correction <- h_gcomp_emm_correction(object, model_mat, grid)
    V_corrected_est <- V + correction$delta

    # Re-expand L_global if h_match_coefs added columns for aliased coefs.
    # Use h_match_coefs (not match) to handle A:B vs B:A reordering.
    L_est <- correction$L_global
    if (ncol(L_est) < orig_ncol && length(aliased_pos) > 0) {
      est_pos <- setdiff(seq_len(orig_ncol), aliased_pos)

      # Expand L_global to full width with zeros for aliased columns.
      L_exp <- matrix(0, nrow = nrow(L_est), ncol = orig_ncol)
      colnames(L_exp) <- orig_colnames
      col_map <- h_match_coefs(colnames(L_est), orig_colnames)
      col_map <- col_map[!is.na(col_map)]
      L_exp[, col_map] <- L_est
      model_mat <- L_exp

      # Expand V to full width with zeros for aliased row/column.
      V_full <- matrix(0, nrow = orig_ncol, ncol = orig_ncol)
      V_full[est_pos, est_pos] <- V_corrected_est
      V <- V_full

      # Zero NA in bhat so 0*NA doesn't propagate.
      beta_hat[aliased_pos] <- 0

      # All grid rows are estimable through L_global (aliased col is zero).
      nbasis <- estimability::all.estble
    } else {
      model_mat <- L_est
      V <- V_corrected_est
    }
  }

  # dffun operates in estimable-coefficient space (p_est dimensions).
  # Strip aliased positions from k before passing to df_md.
  dfargs <- list(
    object = object,
    V_corrected = V_corrected_est,
    aliased_pos = aliased_pos
  )
  dffun <- function(k, dfargs) {
    obj <- dfargs$object
    if (is.null(obj$vcov) || identical(obj$vcov, "Asymptotic")) {
      obj$beta_vcov <- dfargs$V_corrected
    } else {
      obj$beta_vcov_adj <- dfargs$V_corrected
    }
    k_est <- k
    if (
      length(dfargs$aliased_pos) > 0 && length(k) > nrow(dfargs$V_corrected)
    ) {
      k_est <- k[-dfargs$aliased_pos]
    }
    mmrm::df_md(obj, contrast = k_est)$denom_df
  }

  list(
    model_mat = model_mat,
    beta_hat = beta_hat,
    nbasis = nbasis,
    V = V,
    dfargs = dfargs,
    dffun = dffun
  )
}
