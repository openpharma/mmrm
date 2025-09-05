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

#' Conduct type II/III hypothesis testing on the MMRM fit results.
#'
#' @param mod (`mmrm`)\cr the fitted MMRM.
#' @param test.statistic (`string`)\cr either `"F` or `"Chisq"`, indicating the
#'   kind of test to perform.
#' @param vcov. (`matrix` or `function`)\cr the coefficient-covariance matrix or
#'   a function to compute a covariance matrix. This is only used when
#'   `test.statistic = "Chisq"`.
#' @param singular.ok (`flag`)\cr flag indicating whether or not to allow models
#'   with aliased coefficients to undergo `Chisq` testing. Only used if
#'   `test.statistic = "Chisq"`.
#' @param ... Passed to [car::linearHypothesis()] when `test.statistic =
#'   "Chisq"`.
#' @inheritParams h_get_contrast
#'
#' @details `Anova()` will return `anova` object with one row per variable. If
#'   `test.statistic = "F"`, columns will be `Num Df`(numerator degrees of
#'   freedom), `Denom Df` (denominator degrees of freedom), `F Statistic` and
#'   `Pr(>=F)`.
#'
#'   If `test.statistic = "Chisq"`, columns will be `Chisq` (the Chi-squared
#'   test statistic), `Df` (degrees of freedom), and `Pr(>Chisq)` (p-value).
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontshow{
#'
#' Please do not load `car` and then create the documentation. The Rd file will
#' be different.}
Anova.mmrm <- function(mod,
                       type = c("II", "III", "2", "3"),
                       tol = sqrt(.Machine$double.eps),
                       test.statistic = c("F", "Chisq"),
                       vcov. = vcov(mod, complete = FALSE),
                       singular.ok = FALSE,
                       ...) { # nolint
  assert_class(mod, "mmrm")
  type <- as.character(type)
  type <- match.arg(type)
  assert_double(tol, finite = TRUE, len = 1L)
  test.statistic <- match.arg(test.statistic)

  if (test.statistic == "F") {
    vars <- colnames(attr(terms(mod$formula_parts$model_formula), "factors"))
    ret <- lapply(
      vars,
      function(x) df_md(mod, h_get_contrast(mod, x, type, tol))
    )
    ret_df <- do.call(rbind.data.frame, ret)
    row.names(ret_df) <- vars
    colnames(ret_df) <- c("Num Df", "Denom Df", "F Statistic", "Pr(>=F)")
    attr(ret_df, "heading") <-
      sprintf(
        "Analysis of Fixed Effect Table (Type %s %s tests)",
        switch(type, "2" = , "II" = "II", "3" = , "III" = "III"),
        test.statistic
      )
    class(ret_df) <- c("anova", "data.frame")
  } else {
    ret_df <-
      switch(
        type,
        "2" = ,
        II =
          h_Anova_II_Chisq_mmrm(
            mod,
            vcov. = vcov.,
            singular.ok = singular.ok,
            ...
          ),
        "3" = ,
        III =
          h_Anova_III_Chisq_mmrm(
            mod,
            vcov. = vcov.,
            singular.ok = singular.ok,
            ...
          )
      )
  }
  ret_df
}





#' Conduct Type-II or Type-III ANOVA with a Chi-squared Test Statistic.
#'
#' @inheritParams Anova.mmrm
#'
#' @returns `anova` object with one row per variable and columns `Chisq` (the
#'   Chi-squared test statistic), `Df` (degrees of freedom), and `Pr(>Chisq)`
#'   (p-value).
#' @keywords internal
h_Anova_II_Chisq_mmrm <- function(mod, vcov., singular.ok, ...) {
  hypothesis_results <- function(term) {
    which.term <- which(term == names)
    subs.term <- which(assign == which.term)
    relatives <- h_relatives(term, names, fac)
    subs.relatives <- NULL
    for (relative in relatives)
      subs.relatives <- c(subs.relatives, which(assign == relative))
    hyp.matrix.1 <- I.p[subs.relatives, !is_aliased, drop = FALSE]
    hyp.matrix.2 <- I.p[c(subs.relatives, subs.term), !is_aliased, drop = FALSE]
    if (nrow(hyp.matrix.1)) {
      xq <- qr(hyp.matrix.2 %*% vcov. %*% t(hyp.matrix.1))
      if (xq$rank == 0) {
        hyp.matrix.term <- hyp.matrix.2
      } else {
        hyp.matrix.term <-
          t(qr.Q(xq, complete = TRUE)[, -seq_len(xq$rank)]) %*% hyp.matrix.2
      }
    } else {
      hyp.matrix.term <- hyp.matrix.2
    }
    hyp.matrix.term <-
      hyp.matrix.term[
        !apply(hyp.matrix.term, 1, function(x) all(x == 0)), , drop = FALSE
      ]
    if (!nrow(hyp.matrix.term))
      return(c(statistic = NA, df = 0))
    hyp <-
      car::linearHypothesis(
        model = mod,
        hypothesis.matrix = hyp.matrix.term,
        vcov. = vcov.,
        singular.ok = singular.ok,
        ...
      )
    c(statistic = hyp$Chisq[2], df = hyp$Df[2])
  }
  is_aliased <- component(mod, "beta_aliased")
  if (!singular.ok && any(is_aliased))
    stop("there are aliased coefficients in the model")
  terms <- terms(mod)
  fac <- attr(terms, "factors")
  intercept <- attr(terms, "intercept") > 0
  coefs <- coef(mod, complete = TRUE)
  p <- length(coefs)
  I.p <- diag(p)
  assign <- attr(component(mod, "x_matrix"), "assign")
  assign[is_aliased] <- NA
  names <- labels(terms)
  n.terms <- length(names)
  p <- teststat <- df <- rep(0, n.terms)
  for (i in seq_len(n.terms)) {
    hyp <- hypothesis_results(names[i])
    teststat[i] <- abs(hyp["statistic"])
    df[i] <- abs(hyp["df"])
    p[i] <- stats::pchisq(teststat[i], df[i], lower.tail = FALSE)
  }
  result <- data.frame(teststat, df, p)
  row.names(result) <- names
  names(result) <- c("Chisq", "Df", "Pr(>Chisq)")
  class(result) <- c("anova", "data.frame")
  response_name <- deparse1(attr(terms, "variables")[[2L]])
  attr(result, "heading") <-
    c("Analysis of Deviance Table (Type II Wald chisquare tests)\n",
      paste("Response:", response_name))
  result
}


#' Get Indices of a Model Term's "Relatives"
#'
#' For the model term `term`, return the indices of all *other* model terms
#' whose variables contain all of `term`'s variables. These are probably most
#' often the indices of interaction terms whose components contain all `term`'s
#' variables.
#'
#' @param term (`string`)\cr the model term at hand.
#' @param names (`character`)\cr a vector of all the terms in the model.
#' @param factors (`matrix`)\cr the `factors` attribute of a [`terms.object`].
#'
#' @keywords internal
h_relatives <- function(term, names, factors) {
  if (length(names) == 1L) return(NULL)
  term_is_not_name <- is.na(term) | is.na(names) | term != names
  is_relative <-
    vapply(
      names[term_is_not_name],
      function(term2) all(!factors[, term] | factors[, term2]),
      logical(1L)
    )
  seq_along(names)[term_is_not_name][is_relative]
}






#' @rdname h_Anova_II_Chisq_mmrm
h_Anova_III_Chisq_mmrm <- function(mod, vcov., singular.ok, ...) {
  terms <- terms(mod[["formula_parts"]][["model_formula"]])
  intercept <- attr(terms, "intercept") > 0
  p <- length(coef(mod, complete = TRUE))
  I_p <- diag(p)
  names <- c(if (intercept) "(Intercept)", labels(terms))
  n_terms <- length(names)
  assign <- attr(component(mod, "x_matrix"), "assign")
  p <- teststat <- df <- res_df <- rep(0, n_terms)
  if (intercept) df[1] <- 1
  is_aliased <- component(mod, "beta_aliased")
  if (!singular.ok && any(is_aliased)) {
    stop("There cannot be any aliased coefficients when singular.ok = FALSE",
         call. = FALSE)
  }
  for (term in seq_len(n_terms)) {
    subs <- which(assign == term - intercept)
    hyp_matrix <- I_p[subs, !is_aliased, drop = FALSE]
    hyp_matrix <- hyp_matrix[
      !apply(hyp_matrix, MARGIN = 1, function(x) all(x == 0)), , drop = FALSE
    ]
    if (nrow(hyp_matrix)) {
      hyp <-
        car::linearHypothesis(
          model = mod,
          hypothesis.matrix = hyp_matrix,
          vcov. = vcov.,
          singular.ok = singular.ok,
          ...
        )
      teststat[term] <- hyp$Chisq[2]
      df[term] <- abs(hyp$Df[2])
      p[term] <- stats::pchisq(teststat[term], df[term], lower.tail = FALSE)
    } else {
      teststat[term] <- NA
      df[term] <- 0
      p[term] <- NA
    }
  }
  ret_df <- data.frame(teststat, df, p)
  row.names(ret_df) <- names
  names(ret_df) <- c("Chisq", "Df", "Pr(>Chisq)")
  response_name <- deparse1(attr(terms, "variables")[[2L]])
  attr(ret_df, "heading") <-
    c("Analysis of Deviance Table (Type III Wald chisquare tests)\n",
      paste("Response:", response_name))
  class(ret_df) <- c("anova", "data.frame")
  ret_df
}




#' Test a Linear Hypothesis on an `mmrm` Fit
#'
#' @param model (`mmrm`)\cr an `mmrm` model fit.
#' @param hypothesis.matrix (`matrix`)\cr see [car::linearHypothesis()].
#' @param rhs (`numeric`)\cr see [car::linearHypothesis()].
#' @param verbose (`flag`)\cr see [car::linearHypothesis()].
#' @param ... arguments passed down from other methods.
#' @inheritParams Anova.mmrm
#'
#' @keywords internal
linearHypothesis.mmrm <- function(model,
                                  hypothesis.matrix,
                                  rhs = NULL,
                                  vcov. = NULL,
                                  singular.ok = FALSE,
                                  verbose = FALSE,
                                  ...) {
  if (is.null(vcov.)) {
    V <- vcov(model, complete = FALSE)
  } else if (is.function(vcov.)) {
    V <- vcov.(model)
  } else {
    V <- vcov.
  }
  V <- as.matrix(V)
  b <- coef(model, complete = TRUE)
  is_aliased <- component(model, "beta_aliased")
  if (!singular.ok && any(is_aliased)) {
    stop("There cannot be aliased coefficients in the model.", call. = FALSE)
  }
  b <- b[!is_aliased]
  if (is.character(hypothesis.matrix)) {
    L <- car::makeHypothesis(names(b), hypothesis.matrix, rhs)
    if (is.null(dim(L)))
      L <- t(L)
    rhs <- L[, NCOL(L)]
    L <- L[, -NCOL(L), drop = FALSE]
    rownames(L) <- hypothesis.matrix
  } else {
    if (is.null(dim(hypothesis.matrix))) {
      L <- t(hypothesis.matrix)
    } else {
      L <- hypothesis.matrix
    }
    if (is.null(rhs)) rhs <- rep(0, nrow(L))
  }
  q <- NROW(L)
  if (verbose) {
    cat("\nHypothesis matrix:\n")
    print(L)
    cat("\nRight-hand-side vector:\n")
    print(rhs)
    cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
    print(drop(L %*% b - rhs))
    cat("\n")
  }
  SSH <-
    as.vector(
      t(L %*% b - rhs) %*% solve(L %*% V %*% t(L)) %*% (L %*% b - rhs)
    )
  name <- tryCatch(formula(model), error = function(e) substitute(model))
  title <- "\nLinear hypothesis test:"
  topnote <- paste("Model 1: restricted model", "\n", "Model 2: ",
                   paste(deparse1(name), collapse = "\n"), sep = "")
  if (is.null(vcov.)) {
    note <- ""
  } else {
    note <- "\nNote: Coefficient covariance matrix supplied.\n"
  }
  rval <- matrix(rep(NA, 8), ncol = 4)
  colnames(rval) <- c("Res.Df", "Df", "Chisq", paste("Pr(>Chisq)", sep = ""))
  rownames(rval) <- 1:2
  rval[, 1] <- c(Inf + q, Inf)
  p <- stats::pchisq(SSH, q, lower.tail = FALSE)
  rval[2, 2:4] <- c(q, SSH, p)
  rval <- as.data.frame(rval[, -1])
  attr(rval, "heading") <-
    c(title, car::printHypothesis(L, rhs, names(b)), "", topnote, note)
  class(rval) <- c("anova", "data.frame")
  rval
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
