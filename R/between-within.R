#' Determine Within or Between for each Design Matrix Column
#'
#' @description Used in [h_df_bw_calc()] to determine whether a variable
#'   differs only between subjects or also within subjects.
#'
#' @param x_matrix (`matrix`)\cr the design matrix with column names.
#' @param subject_ids (`factor`)\cr the subject IDs.
#'
#' @return Character vector with "intercept", "within" or "between" for each
#'   design matrix column identified via the names of the vector.
#'
#' @keywords internal
h_within_or_between <- function(x_matrix, subject_ids) {
  assert_matrix(x_matrix, col.names = "unique", min.cols = 1L)
  assert_factor(subject_ids, len = nrow(x_matrix))

  n_subjects <- length(unique(subject_ids))
  vapply(
    colnames(x_matrix),
    function(x) {
      if (x == "(Intercept)") {
        "intercept"
      } else {
        n_unique <- nrow(unique(cbind(x_matrix[, x], subject_ids)))
        if (n_unique > n_subjects) "within" else "between"
      }
    },
    character(1L)
  )
}

#' Calculation of Between-Within Degrees of Freedom
#'
#' @description Used in [h_df_1d_bw()] and [h_df_md_bw()].
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#'
#' @return List with:
#'   - `coefs_between_within` calculated via [h_within_or_between()]
#'   - `ddf_between`
#'   - `ddf_within`
#'
#' @keywords internal
h_df_bw_calc <- function(object) {
  assert_class(object, "mmrm")

  n_subjects <- component(object, "n_subjects")
  n_obs  <- component(object, "n_obs")
  x_mat <- component(object, "x_matrix")

  subject_var <- component(object, "subject_var")
  full_frame <- component(object, "full_frame")
  subject_ids <- full_frame[[subject_var]]

  coefs_between_within <- h_within_or_between(x_mat, subject_ids)
  n_coefs_between <- sum(coefs_between_within == "between")
  n_intercept <- sum(coefs_between_within == "intercept")
  n_coefs_within  <- sum(coefs_between_within == "within")
  ddf_between <- n_subjects - n_coefs_between - n_intercept
  ddf_within <- n_obs - n_subjects - n_coefs_within

  list(
    coefs_between_within = coefs_between_within,
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
  df <- if (bw_calc$coefs[as.logical(contrast)] == "within") {
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
