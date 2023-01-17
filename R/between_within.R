#' Check If Variable Changes within Group
#'
#' @description `r lifecycle::badge("experimental")`
#' Check if the variable changes within the same subject.
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param index (`integer`)\cr the index of the `beta_est` to be checked.
#'
#' @return Flag whether there is changed value within subject.
h_between_or_within <- function(object, index) {
  assign_var <- attr(object$tmb_data$x_matrix, "assign")
  terms_formula <- terms(object$formula_parts$model_formula)
  terms_labels <- attr(terms_formula, "term.labels")
  var <- terms_labels[assign_var[index]]
  if (grepl(":", var)) {
    var <- strsplit(var, ":")[[1]]
  }
  within_subject_var <- split(
    object$tmb_data$full_frame[var],
    object$tmb_data$full_frame[[object$formula_parts$subject_var]]
  )
  any_change <- any(
    vapply(
      within_subject_var,
      function(x) {
        nrow(unique(x)) > 1
      },
      FUN.VALUE = TRUE
    )
  )
  return(any_change)
}



#' Calculation of Degrees of Freedom for One-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#' Calculates the estimate, adjusted standard error, degree of freedom,
#' t statistic and p-value for one-dimensional contrast. Used in `[df_1d()]`
#' if method is "Between-within".
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should only include
#' one non-zero value.
#'
#' @return List with `est`, `se`, `df`, `t_stat` and `p_val`.
#' @keywords internal
h_df_1d_bw <- function(object, contrast) {
  assert_class(object, "mmrm")
  n_params <- length(component(object, "beta_est"))
  assert_numeric(contrast, len = n_params)
  index <- which(contrast != 0)
  assert_int(index)
  is_between <- h_between_or_within(fit, index)
  if (bw) {
    df <- component(fit, "n_subjects") - n_params
  } else {
    df <- component(fit, "n_obs") - component(fit, "n_subjects") - n_params
  }
  est <- component(object, "beta_est") %*% contrast
  var <- contrast %*% component(object, "beta_vcov")
  se <- sqrt(var)
  t_stat <- est / se
  pval <- 2 * stats::pt(q = abs(t_stat), df = df, lower.tail = FALSE)
  list(
    est = est,
    se = se,
    df = df,
    t_stat = t_stat,
    p_val = p_val
  )
}


#' Calculation of Degrees of Freedom for One-Dimensional Contrast
#'
#' @description `r lifecycle::badge("experimental")`
#' Calculates the estimate, adjusted standard error, degree of freedom,
#' t statistic and p-value for multi-dimensional contrast. Used in `[df_md()]`
#' if method is "Between-within".
#'
#' @param object (`mmrm`)\cr the MMRM fit.
#' @param contrast (`numeric`)\cr contrast vector. Note that this should only include
#' one non-zero value.
#'
#' @keywords internal
h_df_md_bw <- function(object, contrast) {
  stop("Between-within degree of freedom undefined for multiple dimension scenario!")
}
