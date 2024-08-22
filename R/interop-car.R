#' Register `mmrm` For Use With `car::Anova`
#'
#' @inheritParams base::requireNamespace
#' @return A logical value indicating whether registration was successful.
#'
#' @keywords internal
car_add_mmrm <- function(quietly = FALSE) {
  if (!requireNamespace("car", quietly = quietly)) {
    return(FALSE)
  }
  envir <- asNamespace("mmrm")
  h_register_s3("car", "Anova", "mmrm", envir)
  TRUE
}


#' Obtain Contrast for Specified Effect
#'
#' This is support function to obtain contrast matrix for type II/III testing.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#' @param effect (`string`) the name of the effect.
#' @param type (`string`) type of test, "II", "III", '2', or '3'.
#' @param tol (`numeric`) threshold blow which values are treated as 0.
#'
#' @return A `matrix` of the contrast.
#'
#' @keywords internal
h_get_contrast <- function(object, effect, type = c("II", "III", "2", "3"), tol = sqrt(.Machine$double.eps)) {
  assert_class(object, "mmrm")
  assert_string(effect)
  assert_double(tol, finite = TRUE, len = 1L)
  type <- match.arg(type)
  mx <- component(object, "x_matrix")
  asg <- attr(mx, "assign")
  formula <- object$formula_parts$model_formula
  tms <- terms(formula)
  fcts <- attr(tms, "factors")[-1L, , drop = FALSE] # Discard the response.
  ods <- attr(tms, "order")
  assert_subset(effect, colnames(fcts))
  idx <- which(effect == colnames(fcts))
  cols <- which(asg == idx)
  xlev <- component(object, "xlev")
  contains_intercept <- (!0 %in% asg) && h_first_contain_categorical(effect, fcts, names(xlev))
  coef_rows <- length(cols) - as.integer(contains_intercept)
  l_mx <- matrix(0, nrow = coef_rows, ncol = length(asg))
  if (coef_rows == 0L) {
    return(l_mx)
  }
  if (contains_intercept) {
    l_mx[, cols] <- cbind(-1, diag(rep(1, coef_rows)))
  } else {
    l_mx[, cols] <- diag(rep(1, coef_rows))
  }
  for (i in setdiff(seq_len(ncol(fcts)), idx)) {
    additional_vars <- names(which(fcts[, i] > fcts[, idx]))
    additional_numeric <- any(!additional_vars %in% names(xlev))
    current_col <- which(asg == i)
    if (ods[i] >= ods[idx] && all(fcts[, i] >= fcts[, idx]) && !additional_numeric) {
      sub_mat <- switch(type,
        "2" = ,
        "II" = {
          x1 <- mx[, cols, drop = FALSE]
          x0 <- mx[, -c(cols, current_col), drop = FALSE]
          x2 <- mx[, current_col, drop = FALSE]
          m <- diag(rep(1, nrow(x0))) - x0 %*% solve(t(x0) %*% x0) %*% t(x0)
          ret <- solve(t(x1) %*% m %*% x1) %*% t(x1) %*% m %*% x2
          if (contains_intercept) {
            ret[-1, ] - ret[1, ]
          } else {
            ret
          }
        },
        "3" = ,
        "III" = {
          lvls <- h_obtain_lvls(effect, additional_vars, xlev)
          t_levels <- lvls$total
          nms_base <- colnames(mx)[cols]
          nms <- colnames(mx)[current_col]
          base_idx <- length(lvls$prior) + 1L
          nms_base_values <- vapply(strsplit(nms, ":"), \(x) x[base_idx], FUN.VALUE = "")
          current_row_idx <- match(nms_base_values, nms_base)
          mt <- l_mx[, cols, drop = FALSE] / t_levels
          ret <- mt[, current_row_idx, drop = FALSE]
          # if there is extra levels, replace it with -1/t_levels
          ret[is.na(ret)] <- -1 / t_levels
          ret
        }
      )
      l_mx[, current_col] <- sub_mat
    }
  }
  l_mx[abs(l_mx) < tol] <- 0
  l_mx
}

#' Conduct type II/III hypothesis testing on the MMRM fit results.
#'
#' @param mod (`mmrm`)\cr the fitted MMRM.
#' @param ... not used.
#' @inheritParams h_get_contrast
#'
#' @details
#' `Anova` will return `anova` object with one row per variable and columns
#' `Num Df`(numerator degrees of freedom), `Denom Df`(denominator degrees of freedom),
#' `F Statistic` and `Pr(>=F)`.
#'
#' @keywords internal
# Please do not load `car` and then create the documentation. The Rd file will be different.
Anova.mmrm <- function(mod, type = c("II", "III", "2", "3"), tol = sqrt(.Machine$double.eps), ...) { # nolint
  assert_double(tol, finite = TRUE, len = 1L)
  type <- match.arg(type)
  vars <- colnames(attr(terms(mod$formula_parts$model_formula), "factors"))
  ret <- lapply(
    vars,
    function(x) df_md(mod, h_get_contrast(mod, x, type, tol))
  )
  ret_df <- do.call(rbind.data.frame, ret)
  row.names(ret_df) <- vars
  colnames(ret_df) <- c("Num Df", "Denom Df", "F Statistic", "Pr(>=F)")
  class(ret_df) <- c("anova", "data.frame")
  attr(ret_df, "heading") <- sprintf(
    "Analysis of Fixed Effect Table (Type %s F tests)",
    switch(type,
      "2" = ,
      "II" = "II",
      "3" = ,
      "III" = "III"
    )
  )
  ret_df
}


#' Obtain Levels Prior and Posterior
#' @param var (`string`) name of the effect.
#' @param additional_vars (`character`) names of additional variables.
#' @param xlev (`list`) named list of character levels.
#' @param factors (`matrix`) the factor matrix.
#' @keywords internal
h_obtain_lvls <- function(var, additional_vars, xlev, factors) {
  assert_string(var)
  assert_character(additional_vars)
  assert_list(xlev, types = "character")
  nms <- names(xlev)
  assert_subset(additional_vars, nms)
  if (var %in% nms) {
    prior_vars <- intersect(nms[seq_len(match(var, nms) - 1)], additional_vars)
    prior_lvls <- vapply(xlev[prior_vars], length, FUN.VALUE = 1L)
    post_vars <- intersect(nms[seq(match(var, nms) + 1, length(nms))], additional_vars)
    post_lvls <- vapply(xlev[post_vars], length, FUN.VALUE = 1L)
    total_lvls <- prod(prior_lvls) * prod(post_lvls)
  } else {
    prior_lvls <- vapply(xlev[additional_vars], length, FUN.VALUE = 1L)
    post_lvls <- 2L
    total_lvls <- prod(prior_lvls)
  }
  list(
    prior = prior_lvls,
    post = post_lvls,
    total = total_lvls
  )
}

#' Check if the Effect is the First Categorical Effect
#' @param effect (`string`) name of the effect.
#' @param categorical (`character`) names of the categorical values.
#' @param factors (`matrix`) the factor matrix.
#' @keywords internal
h_first_contain_categorical <- function(effect, factors, categorical) {
  assert_string(effect)
  assert_matrix(factors)
  assert_character(categorical)
  mt <- match(effect, colnames(factors))
  varnms <- row.names(factors)
  # if the effect is not categorical in any value, return FALSE
  if (!any(varnms[factors[, mt] > 0] %in% categorical)) {
    return(FALSE)
  }
  # keep only categorical rows that is in front of the current factor
  factors <- factors[row.names(factors) %in% categorical, seq_len(mt - 1L), drop = FALSE]
  # if previous cols are all numerical, return TRUE
  if (ncol(factors) < 1L) {
    return(TRUE)
  }
  col_ind <- apply(factors, 2, prod)
  # if any of the previous cols are categorical, return FALSE
  return(!any(col_ind > 0))
}
