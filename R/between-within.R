#' Calculation of Between-Within Degrees of Freedom
#'
#' @description Used in [h_df_1d_bw()] and [h_df_md_bw()].
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#'
#' @return List with:
#'   - `pars` (`character` mapping the design matrix columns to "between" or "within")
#'   - `ddf_between`
#'   - `ddf_within`
#'
#' @keywords internal
h_df_bw_calc <- function(object) {
  assert_class(object, "mmrm")

  n_subjects <- component(object, "n_subjects")
  n_obs  <- component(object, "n_obs")
  x_mat <- component(object, "x_matrix")
  n_intercept <- if (any(colnames(x_mat) == "(Intercept)")) 1L else 0L
  x_mat_names <- colnames(x_mat)

  pars <- sapply(X = x_mat_names, function(x) {
    if (x == "(Intercept)") {"within"}
    else {
      n_unique <- nrow(unique(cbind(x_mat[, x], as.numeric(object$tmb_data$full_frame[[object$formula_parts$subject_var]]))))
      if (n_unique > n_subjects) "within"
      else "between"
    }
  })

  n_pars_between <- sum(pars == "between")
  n_pars_within  <- sum(pars == "within") - n_intercept
  ddf_between <- n_subjects - n_pars_between - n_intercept
  ddf_within <- n_obs - n_subjects - n_pars_within

  list(
    pars = pars,
    ddf_between = ddf_between,
    ddf_within = ddf_within
  )
}

#' Calculation of Between-Within Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Used in [df_1d()] if method is "Between-within".
#'
#' @inheritParams h_df_1d_sat
#' @inherit h_df_1d_sat return
#' @keywords internal
h_df_1d_bw <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")))

  bw_calc <- h_df_bw_calc(object)
  df <- if (bw_calc$pars[as.logical(contrast)] == "within") {
    bw_calc$ddf_within
  } else {
    bw_calc$ddf_between
  }
  df <- unname(df)

  h_test_1d(object, contrast, df)
}

#' Calculation of Between-Within Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Used in [df_md()] if method is "Between-within".
#'
#' @inheritParams h_df_md_sat
#' @inherit h_df_md_sat return
#' @keywords internal
h_df_md_bw <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_matrix(contrast, mode = "numeric", any.missing = FALSE, ncols = length(component(object, "beta_est")))

  bw_calc <- h_df_bw_calc(object)
  df <- apply(X = object$tmb_data$x_matrix[, as.logical(colSums(contrast)), drop = FALSE], MARGIN = 2, FUN = function(x) {
    n_unique <- nrow(unique(cbind(x, as.numeric(object$tmb_data$full_frame[[object$formula_parts$subject_var]]))))
    if (n_unique > n_subjects) bw_calc$ddf_within
    else bw_calc$ddf_between
  })
  df <- unname(min(df))

  h_test_md(object, contrast, df)
}
