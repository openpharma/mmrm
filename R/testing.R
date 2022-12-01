#' Calculation of Degrees of Freedom for One-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#' @param ... additional arguments (depending on method)
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @export
#' 
#' @examples
#' object <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' contrast <- numeric(length(object$beta_est))
#' contrast[3] <- 1
#' df_1d(object, contrast)
df_1d <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, len = length(component(object, "beta_est")), any.missing = FALSE)
  contrast <- as.vector(contrast)
  method <- h_get_method(object$method)
  if (method == "Satterthwaite") {
    df_1d_sat(object, contrast)
  } else if (method == "Kenward-Roger") {
    df_1d_kr(object, contrast, linear = FALSE)
  } else if (method == "Kenward-Roger-Linear") {
    df_1d_kr(object, contrast, linear = TRUE)
  }
}


#' Calculation of Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`matrix`)\cr numeric contrast matrix, if given a `numeric`
#'   then this is coerced to a row vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
#' @param ... additional arguments (depending on method)
#' 
#' @return List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
#' @export
#'
#' @examples
#' object <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' contrast <- matrix(data = 0, nrow = 2, ncol = length(object$beta_est))
#' contrast[1, 2] <- contrast[2, 3] <- 1
#' df_md(object, contrast)
df_md <- function(object, contrast, ...) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, any.missing = FALSE)
  if (!is.matrix(contrast)) {
    contrast <- matrix(contrast, ncol = length(contrast))
  }
  assert_matrix(contrast, ncols = length(component(object, "beta_est")))
  if (object$method == "Satterthwaite") {
    df_md_sat(object, contrast)
  } else if (object$method == "Kenward-Roger") {
    df_md_kr(object, matrix(contrast, nrow = 1), ...)
  }
}
