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

#' Obtain Type 2 Contrast for One Specified Effect
#'
#' This is support function to obtain contrast matrix for type II testing.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#' @param effect (`string`) the name of the effect.
#' @param tol (`numeric`) threshold below which values are treated as 0.
#'
#' @return A `matrix` of the contrast.
#'
#' @keywords internal
h_type2_contrast <- function(
  object,
  effect,
  tol = sqrt(.Machine$double.eps)
) {
  assert_class(object, "mmrm")
  assert_string(effect)
  assert_double(tol, finite = TRUE, len = 1L)

  mx <- component(object, "x_matrix")
  asg <- attr(mx, "assign")
  asg_complete <- component(object, "assign_complete")
  aliased <- component(object, "beta_aliased")
  formula <- object$formula_parts$model_formula
  tms <- stats::terms(formula)
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
    if (
      ods[i] >= ods[idx] && all(fcts[, i] >= fcts[, idx]) && !additional_numeric
    ) {
      x1 <- mx[, cols, drop = FALSE]
      x0 <- mx[, -c(cols, current_col), drop = FALSE]
      x2 <- mx[, current_col, drop = FALSE]
      m <- diag(rep(1, nrow(x0))) - x0 %*% solve(t(x0) %*% x0) %*% t(x0)
      ret <- solve(t(x1) %*% m %*% x1) %*% t(x1) %*% m %*% x2
      sub_mat <- if (effect_contains_intercept) {
        ret[-1, ] - ret[1, ]
      } else {
        ret
      }
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



#' Obtain Type 3 Contrast for All Effects
#'
#' This is support function to obtain contrast matrices for type III testing.
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#' @param tol (`numeric`) threshold below which values are treated as 0.
#'
#' @return A `list` of contrast matrices, one per effect.
#' @keywords internal
h_type3_contrasts <- function(object, tol = sqrt(.Machine$double.eps)) {
  assert_class(object, "mmrm")
  assert_double(tol, finite = TRUE, len = 1L)

  # First obtain the simple contrasts as if we use all contr.sum only.
  contr_sum_contrasts <- h_contr_sum_type3_contrasts(object)

  # The original design matrix.
  orig_model_matrix <- component(object, "x_matrix")

  # Obtain original contrasts, if there is none or all are contr.sum, return
  # the contr.sum contrasts.
  orig_contrasts <- attr(orig_model_matrix, "contrasts")
  if (
    length(orig_contrasts) == 0L ||
      all(vapply(orig_contrasts, identical, logical(1L), "contr.sum"))
  ) {
    return(contr_sum_contrasts)
  }

  # Compute the modified design matrix using all contr.sum.
  new_contrasts <- orig_contrasts
  new_contrasts[] <- "contr.sum"
  orig_data <- component(object, "full_frame")
  new_model_matrix <-
    stats::model.matrix(
      object,
      data = orig_data,
      contrasts = new_contrasts
    )
  coef_aliased <- component(object, "beta_aliased")
  if (any(coef_aliased)) {
    new_model_matrix <- new_model_matrix[, !coef_aliased, drop = FALSE]
  }

  # Compute the correction matrix and return corrected contrasts,
  # dropping small values.
  type_iii_correction <-
    solve(crossprod(new_model_matrix)) %*%
    crossprod(new_model_matrix, orig_model_matrix)
  result <- lapply(contr_sum_contrasts, `%*%`, type_iii_correction)
  lapply(result, function(mat) {
    mat[abs(mat) < tol] <- 0
    mat
  })
}

#' Conduct type II/III hypothesis testing on the MMRM fit results.
#'
#' @param mod (`mmrm`)\cr the fitted MMRM.
#' @param type (`string`)\cr either `"II"`, `"III"`, `"2"`, or `"3"`, indicating the
#'   type of test to perform.
#' @param test.statistic (`string`)\cr either `"F` or `"Chisq"`, indicating the
#'   kind of test to perform.
#' @param ... arguments passed from other methods.
#' @inheritParams h_type2_contrast
#'
#' @details `Anova()` will return an `anova` object with one row per variable.
#'
#'   If `test.statistic = "F"`, columns will be `Df`(numerator degrees of
#'   freedom), `Res.Df` (denominator degrees of freedom), `F`, and
#'   `Pr(>F)`.
#'
#'   If `test.statistic = "Chisq"`, columns will be `Chisq` (the Chi-squared
#'   test statistic), `Df` (degrees of freedom), and `Pr(>Chisq)` (p-value).
#'
#' @keywords internal
# Please do not load `car` and then create the documentation. The Rd file will
#  be different.
# nolint start
Anova.mmrm <- function(
  # nolint end
  mod,
  type = c("II", "III", "2", "3"),
  tol = sqrt(.Machine$double.eps),
  test.statistic = c("F", "Chisq"), # nolint
  ...
) {
  type <- as.character(type)
  type <- match.arg(type)
  test.statistic <- match.arg(test.statistic) # nolint

  vars <- colnames(attr(terms(mod$formula_parts$model_formula), "factors"))
  if (type %in% c("II", "2")) {
    contrasts <- lapply(vars, h_type2_contrast, object = mod, tol = tol)
    names(contrasts) <- vars
  } else {
    contrasts <- h_type3_contrasts(mod, tol = tol)
    contrasts <- contrasts[intersect(vars, names(contrasts))]
  }

  if (test.statistic == "F") {
    ret <- lapply(contrasts, df_md, object = mod)
    ret_df <- do.call(rbind.data.frame, ret)
    colnames(ret_df) <- c("Df", "Res.Df", "F", "Pr(>F)")
  } else {
    ret <- lapply(contrasts, h_test_md, object = mod, test = "Chisq")
    ret_df <- do.call(rbind.data.frame, ret)
    colnames(ret_df) <- c("Df", "Chisq", "Pr(>Chisq)")
  }

  row.names(ret_df) <- names(contrasts)
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
    post_vars <- intersect(
      nms[seq(match(var, nms) + 1, length(nms))],
      additional_vars
    )
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
  factors <- factors[
    row.names(factors) %in% categorical,
    seq_len(mt - 1L),
    drop = FALSE
  ]
  # if previous cols are all numerical, return TRUE
  if (ncol(factors) < 1L) {
    return(TRUE)
  }
  col_ind <- apply(factors, 2, prod)
  # if any of the previous cols are categorical, return FALSE
  !any(col_ind > 0)
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

#' Construct Preliminary Contrast Matrices for Type III Tests Assuming Sum Contrasts
#'
#' @param object (`mmrm`)\cr the fitted MMRM.
#'
#' @return A `list` of contrast matrices, which are just row-wise term subsets of
#'   an overall identity matrix.
#'
#' @keywords internal
h_contr_sum_type3_contrasts <- function(object) {
  assert_class(object, "mmrm")

  terms <- stats::terms(object)
  has_intercept <- attr(terms, "intercept")
  if (!has_intercept) {
    warning(
      "Type III tests can give misleading results for models without an intercept"
    )
  }
  term_labels <- c(if (has_intercept) "(Intercept)", labels(terms))
  n_coefs <- length(component(object, "beta_est"))
  identity_matrix <- diag(n_coefs)

  x_matrix <- component(object, "x_matrix")
  assign <- attr(x_matrix, "assign")
  assert_integer(assign, lower = 0L, len = ncol(x_matrix))
  map_terms_to_cols <- assign + has_intercept

  terms_per_col <- factor(term_labels[map_terms_to_cols], levels = term_labels)

  split.data.frame(
    identity_matrix,
    terms_per_col,
    drop = TRUE
  )
}
