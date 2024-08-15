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
  has_intercept <- !0 %in% asg
  formula <- object$formula_parts$model_formula
  tms <- terms(formula)
  fcts <- attr(tms, "factors")[-1L, , drop = FALSE] # Discard the response.
  ods <- attr(tms, "order")
  assert_subset(effect, colnames(fcts))
  idx <- which(effect == colnames(fcts))
  cols <- which(asg == idx)
  xlev <- component(object, "xlev")
  is_intercet <- has_intercept && identical(names(xlev)[1], effect)
  coef_rows <- length(cols) - is_intercet
  l_mx <- matrix(0, nrow = coef_rows, ncol = length(asg))
  if (coef_rows == 0L) {
    return(l_mx)
  }
  if (is_intercet) {
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
          if (is_intercet) {
            ret[-1, ] - ret[1, ]
          } else {
            ret
          }
        },
        "3" = ,
        "III" = {
          lvls <- h_obtain_lvls(effect, additional_vars, xlev)
          t_levels <- lvls$total
          current_lvls <- length(current_col) / prod(c(lvls$prior, lvls$post) - 1L)
          current_row_idx <- rep(rep(seq_len(current_lvls), each = prod(lvls$prior - 1L)), times = prod(lvls$post - 1L))
          mt <- matrix(0, nrow = current_lvls, ncol = length(current_col))
          mt[cbind(current_row_idx, seq_len(length(current_row_idx)))] <- 1 / t_levels
          mt
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
#' @keywords internal
h_obtain_lvls <- function(var, additional_vars, xlev) {
  assert_string(var)
  assert_character(additional_vars)
  assert_list(xlev, types = "character")
  nms <- names(xlev)
  assert_subset(additional_vars, nms)
  if (var %in% nms) {
    prior_vars <- intersect(nms[seq_len(match(var, nms) - 1)], additional_vars)
    prior_lvls <- vapply(xlev[prior_vars], length, FUN.VALUE = 1L)
    post_vars <- intersect(nms[seq(match(var, nms), length(nms))], additional_vars)
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
