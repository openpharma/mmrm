#' Calculation of Degrees of Freedom for One-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#' Calculates the estimate, adjusted standard error, degree of freedom,
#' t statistic and p-value for one-dimensional contrast.
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
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
  if (identical(object$method, "Satterthwaite")) {
    h_df_1d_sat(object, contrast)
  } else if (identical(object$method, "Kenward-Roger")) {
    h_df_1d_kr(object, contrast)
  } else if (identical(object$method, "Between-within")) {
    h_df_1d_bw(object, contrast)
  }
}


#' Calculation of Degrees of Freedom for Multi-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#' Calculates the estimate, standard error, degree of freedom,
#' t statistic and p-value for one-dimensional contrast, depending on the method
#' used in [mmrm()].
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`matrix`)\cr numeric contrast matrix, if given a `numeric`
#'   then this is coerced to a row vector. Note that this should not include
#'   elements for singular coefficient estimates, i.e. only refer to the
#'   actually estimated coefficients.
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
df_md <- function(object, contrast) {
  assert_class(object, "mmrm")
  assert_numeric(contrast, any.missing = FALSE)
  if (!is.matrix(contrast)) {
    contrast <- matrix(contrast, ncol = length(contrast))
  }
  assert_matrix(contrast, ncols = length(component(object, "beta_est")))
  if (identical(object$method, "Satterthwaite")) {
    h_df_md_sat(object, contrast)
  } else if (identical(object$method, "Kenward-Roger")) {
    h_df_md_kr(object, contrast)
  } else if (identical(object$method, "Between-within")) {
    h_df_md_bw(object, contrast)
  }
}
