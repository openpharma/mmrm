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
  asg_complete <- component(object, "assign_complete")
  aliased <- component(object, "beta_aliased")
  formula <- object$formula_parts$model_formula
  tms <- terms(formula)
  fcts <- attr(tms, "factors")[-1L, , drop = FALSE] # Discard the response.
  ods <- attr(tms, "order")
  assert_subset(effect, colnames(fcts))
  idx <- which(effect == colnames(fcts))
  cols <- which(asg == idx)
  xlev <- component(object, "xlev")

  categorical_covars <- intersect(rownames(fcts), names(xlev))

  term_contains_aliased_coefs <- any(aliased[asg_complete == idx])

  effect_contains_intercept <-
    !attr(tms, "intercept") &&
    !term_contains_aliased_coefs &&
    length(categorical_covars) &&
    effect == h_first_term_containing_categorical_var(fcts, categorical_covars)

  coef_rows <- length(cols) - as.integer(effect_contains_intercept)
  l_mx <-
    matrix(
      0,
      nrow = coef_rows,
      ncol = ncol(mx),
      dimnames = list(NULL, colnames(mx))
    )
  if (coef_rows == 0L) {
    return(l_mx)
  }
  if (effect_contains_intercept) {
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
          if (effect_contains_intercept) {
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
          nms_base_split <- strsplit(nms_base, ":")
          nms_split <- strsplit(nms, ":")
          base_idx <- h_get_index(nms_split, nms_base_split)
          mt <- l_mx[, cols, drop = FALSE] / t_levels
          ret <- mt[, base_idx, drop = FALSE]
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




h_first_term_containing_categorical_var <- function(factors, categorical) {
  factors <- factors[categorical, , drop = FALSE]
  for (term in colnames(factors))
    if (any(factors[, term] > 0))
      return(term)
}





#' Conduct type II/III hypothesis testing on the MMRM fit results.
#'
#' @param mod (`mmrm`)\cr the fitted MMRM.
#' @param test.statistic (`string`)\cr either `"F` or `"Chisq"`, indicating the
#'   kind of test to perform.
#' @param ... arguments passed from other methods.
#' @inheritParams h_get_contrast
#'
#' @details `Anova()` will return an `anova` object with one row per variable.
#'
#'   If `test.statistic = "F"`, columns will be `Num Df`(numerator degrees of
#'   freedom), `Denom Df` (denominator degrees of freedom), `F Statistic`, and
#'   `Pr(>=F)`.
#'
#'   If `test.statistic = "Chisq"`, columns will be `Chisq` (the Chi-squared
#'   test statistic), `Df` (degrees of freedom), and `Pr(>=Chisq)` (p-value).
#'
#' @keywords internal
# Please do not load `car` and then create the documentation. The Rd file will
#  be different.
Anova.mmrm <- function(mod, # nolint
                       type = c("II", "III", "2", "3"),
                       tol = sqrt(.Machine$double.eps),
                       test.statistic = c("F", "Chisq"), # nolint
                       ...) {
  type <- as.character(type)
  type <- match.arg(type)
  test.statistic <- match.arg(test.statistic) # nolint

  vars <- colnames(attr(terms(mod$formula_parts$model_formula), "factors"))
  contrasts <-
    lapply(vars, h_get_contrast, object = mod, type = type, tol = tol)

  if (test.statistic == "F") {
    ret <- lapply(contrasts, df_md, object = mod)
    ret_df <- do.call(rbind.data.frame, ret)
    colnames(ret_df) <- c("Num Df", "Denom Df", "F Statistic", "Pr(>=F)")
  } else {
    ret <- lapply(contrasts, h_test_md, object = mod, test = "Chisq")
    ret_df <- do.call(rbind.data.frame, ret)
    colnames(ret_df) <- c("Df", "Chisq Statistic", "Pr(>=Chisq)")
  }

  row.names(ret_df) <- vars
  attr(ret_df, "heading") <-
    sprintf(
      "Analysis of Fixed Effect Table (Type %s %s tests)",
      switch(type, "2" = "II", "3" = "III", type),
      test.statistic
    )
  class(ret_df) <- c("anova", "data.frame")

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
  categorical <- intersect(row.names(factors), categorical)
  # Keep only the categorical var rows. Remove cols after the effect's col.
  factors <- factors[categorical, seq_len(mt), drop = FALSE]
  # TRUE when effect contains categorical vars and all preceding effects do not.
  any(factors[, effect] > 0) && all(factors[, -mt] <= 0)
}

#' Test if the First Vector is Subset of the Second Vector
#' @param x (`vector`) the first list.
#' @param y (`vector`) the second list.
#' @keywords internal
h_get_index <- function(x, y) {
  assert_list(x)
  assert_list(y)
  vapply(
    x,
    \(i) {
      r <- vapply(y, \(j) test_subset(j, i), FUN.VALUE = TRUE)
      if (sum(r) == 1L) {
        which(r)
      } else {
        NA_integer_
      }
    },
    FUN.VALUE = 1L
  )
}
