#' G-computation Covariance Correction
#'
#' When the MMRM includes covariate-by-treatment interactions, the standard
#' emmeans output conditions on covariates rather than averaging over them.
#' These internal helpers compute subject-level potential outcomes under each
#' treatment and use their distribution to correct the L matrix and covariance.
#'
#' @keywords internal
#' @noRd
NULL

#' Get Subject-Level Covariate Data
#'
#' @description Returns one row per subject with covariate data. Uses the
#'   stored `emmeans_gcomp_subject_data` from the original dataset when
#'   available (includes subjects with missing outcomes), otherwise falls
#'   back to the fitted model's complete case frame.
#'
#' @note If time varying covariates are present, only the first observed
#'   value per subject is retained. The intended use is for baseline
#'   (pre-randomization) covariates that are constant across visits.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#'
#' @return A `data.frame` with one row per subject.
#'
#' @keywords internal
#' @noRd
h_get_subject_data <- function(object) {
  assert_class(object, "mmrm")
  original_data <- object$tmb_data$data
  subject_var <- object$formula_parts$subject_var
  model_vars <- all.vars(object$formula_parts$model_formula)
  keep_vars <- intersect(c(subject_var, model_vars), names(original_data))
  subj_data <- original_data[!duplicated(original_data[[subject_var]]), keep_vars, drop = FALSE]
  rownames(subj_data) <- NULL
  subj_data
}

#' Compute Subject-Level Potential Outcomes
#'
#' @description For a given visit, computes each subject's fitted potential
#'   outcome under each combination of fixed variable levels by constructing
#'   the counterfactual design matrix. Each subject's own covariate values
#'   are used for all non-fixed variables.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#' @param subj_data (`data.frame`)\cr one row per subject with covariates.
#' @param visit_value (`string`)\cr the visit level to evaluate at.
#' @param visit_var (`string`)\cr name of the visit variable.
#' @param counterfactual_grid (`data.frame`)\cr unique combinations of fixed
#'   variables (excluding visit) to loop over.
#'
#' @return A list with:
#'   - `vhat`: `matrix` of fitted potential outcomes (n_subjects x K),
#'     where n_subjects is the number of rows in `subj_data` and K is the
#'     number of rows in `counterfactual_grid`.
#'   - `L_global`: `matrix` of globally weighted contrast rows (K x p),
#'     where p is the number of estimable model coefficients.
#'
#' @keywords internal
#' @noRd
h_compute_potential_outcomes <- function(
  object,
  subj_data,
  visit_value,
  visit_var,
  counterfactual_grid
) {
  assert_class(object, "mmrm")
  assert_data_frame(subj_data, min.rows = 1L)
  assert_string(visit_value)
  assert_string(visit_var)
  assert_data_frame(counterfactual_grid, min.rows = 1L)
  beta_hat <- component(object, "beta_est")
  beta_clean <- beta_hat
  beta_clean[is.na(beta_clean)] <- 0
  contrasts_arg <- component(object, "contrasts")
  full_frame <- object$tmb_data$full_frame

  model_formula <- object$formula_parts$model_formula
  model_terms <- stats::delete.response(stats::terms(model_formula))

  K <- nrow(counterfactual_grid)
  n_subj <- nrow(subj_data)
  p <- length(beta_hat)

  # Stack all K counterfactual assignments into one data frame.
  # (for efficiency reasons, we can just do one model.frame and model.matrix call below instead of K).
  cf_list <- vector("list", K)
  for (k in seq_len(K)) {
    subj_cf <- subj_data
    subj_cf[[visit_var]] <- factor(
      visit_value, levels = levels(full_frame[[visit_var]])
    )
    for (var_name in names(counterfactual_grid)) {
      val <- counterfactual_grid[[var_name]][k]
      subj_cf[[var_name]] <- if (is.factor(full_frame[[var_name]])) {
        factor(val, levels = levels(full_frame[[var_name]]))
      } else {
        val
      }
    }
    cf_list[[k]] <- subj_cf
  }
  cf_stacked <- do.call(rbind, cf_list)

  mf <- stats::model.frame(model_terms, data = cf_stacked, na.action = stats::na.pass)
  X_all <- stats::model.matrix(model_terms, data = mf, contrasts.arg = contrasts_arg)
  X_all <- X_all[, names(beta_hat), drop = FALSE]
  X_all[is.na(X_all)] <- 0

  # Split back into K blocks of n_subj rows.
  vhat <- matrix(NA_real_, nrow = n_subj, ncol = K)
  L_global <- matrix(NA_real_, nrow = K, ncol = p)
  for (k in seq_len(K)) {
    rows <- ((k - 1L) * n_subj + 1L):(k * n_subj)
    X_k <- X_all[rows, , drop = FALSE]
    vhat[, k] <- as.vector(X_k %*% beta_clean)
    L_global[k, ] <- colMeans(X_k, na.rm = TRUE)
  }

  list(vhat = vhat, L_global = L_global)
}

#' Compute Per Visit Variance Contributions and Global L Matrix
#'
#' @description For each visit, computes subject level potential outcomes,
#'   the sample covariance of those outcomes (divided by n), and the
#'   globally weighted L matrix rows. Assembles these into the full
#'   block diagonal S matrix and global L matrix across all visits.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#' @param model_mat (`matrix`)\cr the L matrix from the emmeans reference
#'   grid (n_grid x p).
#' @param grid (`data.frame`)\cr the emmeans reference grid.
#' @param fixed_vars (`character`)\cr names of the fixed (intervention)
#'   variables.
#' @param visit_var (`string`)\cr name of the visit variable.
#'
#' @return A list with:
#'   - `S_full`: `matrix` (n_grid x n_grid) block-diagonal sample covariance
#'     of potential outcomes divided by n, with blocks corresponding to visits.
#'   - `L_global`: `matrix` (n_grid x p) globally-weighted L matrix rows.
#'
#' @keywords internal
#' @noRd
h_gcomp_visit_contributions <- function(object, model_mat, grid,
                                         fixed_vars, visit_var) {
  subj_data <- h_get_subject_data(object)
  full_frame <- object$tmb_data$full_frame
  n_subj <- nrow(subj_data)
  visit_levels <- levels(full_frame[[visit_var]])
  cell_vars <- setdiff(fixed_vars, visit_var)

  n_grid <- nrow(model_mat)
  grid_visit <- as.character(grid[[visit_var]])
  S_full <- matrix(0, nrow = n_grid, ncol = n_grid)
  L_global <- model_mat

  for (v in visit_levels) {
    v_mask <- grid_visit == v
    if (!any(v_mask)) next
    v_indices <- which(v_mask)

    grid_at_v <- grid[v_indices, cell_vars, drop = FALSE]
    cf_grid <- unique(grid_at_v)
    rownames(cf_grid) <- NULL

    po <- h_compute_potential_outcomes(
      object = object,
      subj_data = subj_data,
      visit_value = v,
      visit_var = visit_var,
      counterfactual_grid = cf_grid
    )

    sigma_v <- stats::cov(po$vhat, use = "pairwise.complete.obs")
    S_t <- sigma_v / n_subj

    cf_keys <- apply(cf_grid, 1, function(r) paste(as.character(r), collapse = "|"))
    grid_keys <- apply(grid_at_v, 1, function(r) paste(as.character(r), collapse = "|"))

    for (ii in seq_along(v_indices)) {
      ki <- match(grid_keys[ii], cf_keys)
      if (is.na(ki)) next

      L_global[v_indices[ii], ] <- po$L_global[ki, ]

      for (jj in seq_along(v_indices)) {
        kj <- match(grid_keys[jj], cf_keys)
        if (is.na(kj)) next
        S_full[v_indices[ii], v_indices[jj]] <- S_t[ki, kj]
      }
    }
  }

  list(S_full = S_full, L_global = L_global)
}

#' Compute G-computation Correction for emmeans Integration
#'
#' @description Computes the corrected L matrix (global covariate weighting)
#'   and the variance correction matrix delta. Together these produce the
#'   G-computation estimator with correct standard errors. Delta is a
#'   p x p positive semi-definite matrix (where p is the number of estimable
#'   model coefficients) that is added to the model-based covariance.
#'
#' @param object (`mmrm`)\cr the fitted MMRM with
#'   `emmeans_gcomp_vars` set.
#' @param model_mat (`matrix`)\cr the L matrix from the emmeans reference
#'   grid (n_grid x p).
#' @param grid (`data.frame`)\cr the reference grid from emmeans.
#'
#' @return A list with:
#'   - `delta`: `matrix` (p x p) positive semi-definite correction, where
#'     p is the number of estimable model coefficients.
#'   - `L_global`: `matrix` (n_grid x p) globally-weighted L matrix.
#'
#' @keywords internal
#' @noRd
h_gcomp_emm_correction <- function(object, model_mat, grid) {
  assert_class(object, "mmrm")
  assert_matrix(model_mat, mode = "numeric")
  assert_data_frame(grid)

  fixed_vars <-  attr(object$tmb_data, "emmeans_gcomp_vars")
  visit_var <- object$formula_parts$visit_var

  assert_character(fixed_vars, min.len = 1L)
  assert_string(visit_var)

  beta_names <- names(component(object, "beta_est"))
  # The emmeans grid model matrix may have extra columns (e.g. from
  # interaction reordering) that do not appear in beta_est. Trim to
  # match the coefficient vector, since estimability::nonest.basis
  # only handles rank deficiency within the fitted coefficient set.
  if (ncol(model_mat) > length(beta_names)) {
    keep_cols <- match(beta_names, colnames(model_mat))
    keep_cols <- keep_cols[!is.na(keep_cols)]
    model_mat <- model_mat[, keep_cols, drop = FALSE]
  }

  contributions <- h_gcomp_visit_contributions(
    object, model_mat, grid, fixed_vars, visit_var
  )
  S_full <- contributions$S_full
  L <- contributions$L_global

  llt_ginv_l <- MASS::ginv(tcrossprod(L)) %*% L
  delta <- t(llt_ginv_l) %*% S_full %*% llt_ginv_l
  V <- component(object, "beta_vcov")
  dimnames(delta) <- dimnames(V)

  list(delta = delta, L_global = L)
}
