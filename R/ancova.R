#' Anova of mmrm
#' @importFrom car Anova
#' @export Anova
#' @param vcov. variance-covariance matrix (usually extracted automatically)
#' @param test.statistic unused: only valid choice is "Chisq" (i.e., Wald chi-squared test)
#' @param singular.ok OK to do ANOVA with singular models (unused) ?
#' @param type  type of test, \code{"II"}, \code{"III"}, \code{2}, or \code{3}.
#' Roman numerals are equivalent to the corresponding Arabic numerals.
#' See \code{\link[car]{Anova}} for details.
#' @param include.rankdef.cols include all columns of a rank-deficient model matrix?

Anova.mmrm_tmb <- function(mod, type = c("II", "III", 2, 3), # nolint
                           test.statistic = c("Chisq", "F"), # nolint
                           component = "cond",
                           vcov. = vcov(mod)[[component]], # nolint
                           singular.ok, # nolint
                           include.rankdef.cols = FALSE, # nolint
                           ...) {
  test_statistic <- match.arg(test.statistic)
  if (test.statistic == "F") {
    stop("F tests currently unavailable")
  }
  if (is.function(vcov.)) vcov. <- vcov.(mod) # nolint
  type <- as.character(type)
  type <- match.arg(type)
  if (missing(singular.ok)) singular_ok <- type == "2" || type == "II"
  afun <- switch(type,
    `2` = ,
    II = anova_ii,
    `3` = ,
    III = anova_iii
  )
  afun(mod, vcov.,
    test = test_statistic, singular.ok = singular_ok,
    component = component, include.rankdef.cols = include.rankdef.cols
  )
}

anova_ii <- function(mod, vcov., singular.ok = TRUE, test = "Chisq", #
                             component = "cond", include.rankdef.cols = FALSE, ...) {
  ## would feel cleaner to have this external, but it uses
  ##  lots of variable from the function environment ...
  hyp.term <- function(term) {
    which.term <- which(term == names)
    subs.term <- which(assign == which.term)
    relatives <- relatives(term, names, fac)
    subs.relatives <- NULL
    for (relative in relatives) {
      subs.relatives <- c(subs.relatives, which(assign == relative))
    }
    hyp.matrix.1 <- I.p[subs.relatives, , drop = FALSE]
    hyp.matrix.1 <- hyp.matrix.1[, not.aliased, drop = FALSE]
    hyp.matrix.2 <- I.p[c(subs.relatives, subs.term), , drop = FALSE]
    hyp.matrix.2 <- hyp.matrix.2[, not.aliased, drop = FALSE]
    hyp.matrix.term <- if (nrow(hyp.matrix.1) == 0) {
      hyp.matrix.2
    } else {
      t(ConjComp(
        t(hyp.matrix.1),
        t(hyp.matrix.2), vcov.
      ))
    }
    hyp.matrix.term <- hyp.matrix.term[!apply(
      hyp.matrix.term, 1,
      function(x) all(x == 0)
    ), , drop = FALSE]
    if (nrow(hyp.matrix.term) == 0) {
      return(c(statistic = NA, df = 0))
    }
    hyp <- linearHypothesis_glmmTMB(mod, hyp.matrix.term,
      vcov. = vcov.,
      singular.ok = singular.ok,
      test = test,
      component = component, ...
    )
    if (test == "Chisq") {
      return(c(statistic = hyp$Chisq[2], df = hyp$Df[2]))
    } else {
      return(c(statistic = hyp$F[2], df = hyp$Df[2], res.df = hyp$Res.Df[2]))
    }
  } ## hyp.term()

  not.aliased <- !is.na(fixef(mod)[[component]])
  if (!singular.ok && !all(not.aliased)) {
    stop("there are aliased coefficients in the model")
  }
  fac <- attr(terms(mod, component = component), "factors")
  intercept <- has.intercept(mod)
  p <- length(fixef(mod)[[component]])
  I.p <- diag(p)
  ## FIXME:: missing or !missing ???
  if (missing(vcov.)) {
    vcov. <- vcov(mod, complete = FALSE)[[component]]
  }
  vcov. <- vcov.[not.aliased, not.aliased]
  assign <- attr(model.matrix(mod, component = component, include_rankdef = include.rankdef.cols), "assign")
  assign[!not.aliased] <- NA
  names <- term.names.default(mod, component = component)
  if (intercept) names <- names[-1]
  n.terms <- length(names)
  p <- teststat <- df <- res.df <- rep(0, n.terms)
  for (i in seq_len(n.terms)) {
    hyp <- hyp.term(names[i])
    teststat[i] <- abs(hyp["statistic"])
    df[i] <- abs(hyp["df"])
    res.df[i] <- hyp["res.df"]
    p[i] <- pchisq(teststat[i], df[i], lower.tail = FALSE)
  }
  result <- data.frame(teststat, df, p)
  row.names(result) <- names
  names(result) <- c("Chisq", "Df", "Pr(>Chisq)")
  class(result) <- c("anova", "data.frame")
  attr(result, "heading") <- c(
    "Analysis of Deviance Table (Type II Wald chisquare tests)\n",
    paste("Response:", responseName.default(mod))
  )
  return(result)
}

Anova.III.glmmTMB <- function(mod, vcov., singular.ok = FALSE, test = "Chisq",
                              component = "cond", include.rankdef.cols = include.rankdef.cols, ...) {
  intercept <- has.intercept(mod)
  p <- length(fixef(mod)[[component]])
  I.p <- diag(p)
  names <- term.names.default(mod, component = component)
  n.terms <- length(names)
  assign <- attr(model.matrix(mod, component = component, include.rankdef.cols = include.rankdef.cols), "assign")
  p <- teststat <- df <- res.df <- rep(0, n.terms)
  if (intercept) df[1] <- 1
  not.aliased <- !is.na(fixef(mod)[[component]])
  if (!singular.ok && !all(not.aliased)) {
    stop("there are aliased coefficients in the model")
  }
  if (missing(vcov.)) {
    vcov. <- vcov(mod, complete = FALSE)[[component]]
  }
  vcov. <- vcov.[not.aliased, not.aliased]
  assign <- attr(model.matrix(mod, component = component, include_rankdef = include.rankdef.cols), "assign")
  assign[!not.aliased] <- NA
  for (term in seq_len(n.terms)) {
    subs <- which(assign == term - intercept)
    hyp.matrix <- I.p[subs, , drop = FALSE]
    hyp.matrix <- hyp.matrix[, not.aliased, drop = FALSE]
    hyp.matrix <- hyp.matrix[!apply(hyp.matrix, 1, function(x) all(x == 0)), , drop = FALSE]
    if (nrow(hyp.matrix) == 0) {
      teststat[term] <- NA
      df[term] <- 0
      p[term] <- NA
    } else {
      hyp <- linearHypothesis_glmmTMB(mod, hyp.matrix,
        test = test,
        vcov. = vcov., singular.ok = singular.ok,
        component = component, ...
      )
      teststat[term] <- hyp$Chisq[2]
      df[term] <- abs(hyp$Df[2])
      p[term] <- pchisq(teststat[term], df[term], lower.tail = FALSE)
    }
    result <- data.frame(teststat, df, p)
    row.names(result) <- names
    names(result) <- c("Chisq", "Df", "Pr(>Chisq)")
    class(result) <- c("anova", "data.frame")
    attr(result, "heading") <- c(
      "Analysis of Deviance Table (Type III Wald chisquare tests)\n",
      paste("Response:", responseName.default(mod))
    )
  }
  result
}

linearHypothesis_glmmTMB <- function(model, hypothesis.matrix,
                                     rhs = NULL, test = c("Chisq", "F"),
                                     vcov. = NULL, singular.ok = FALSE, verbose = FALSE,
                                     coef. = NULL, component = "cond", ...) {
  ## what's the least ugly way to do this?
  ## match.call?
  test <- match.arg(test)
  ## call linearHypothesis.default (not exported)
  if (!requireNamespace("car")) {
    stop("please install (if necessary) and load the car package")
  }
  if (utils::packageVersion("car") < "3.0.6") {
    stop("please install a more recent version of the car package (>= 3.0.6)")
  }
  car::linearHypothesis(
    model = model,
    hypothesis.matrix = hypothesis.matrix,
    rhs = rhs,
    test = test,
    vcov. = vcov.,
    singular.ok = singular.ok,
    verbose = verbose,
    coef. = fixef(model)[[component]],
    ...
  )
}
