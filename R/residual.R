#' Calculation of Residual Degrees of Freedom for One-Dimensional Contrast
#'
#' @description Used in [df_1d()] if method is
#' "Residual".
#'
#' @inheritParams h_df_1d_sat
#' @inherit h_df_1d_sat return
#' @keywords internal
h_df_1d_res <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")))

  df <- component(object, "n_obs") - length(component(object, "beta_est"))

  h_test_1d(object, contrast, df)
}

#' Calculation of Residual Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description Used in [df_md()] if method is "Residual".
#'
#' @inheritParams h_df_md_sat
#' @inherit h_df_md_sat return
#' @keywords internal
h_df_md_res <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_matrix(contrast, mode = "numeric", any.missing = FALSE, ncols = length(component(object, "beta_est")))

  df <- component(object, "n_obs") - length(component(object, "beta_est"))

  h_test_md(object, contrast, df)
}
