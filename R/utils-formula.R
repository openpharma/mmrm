#' Extract Formula Terms used for Covariance Structure Definition
#'
#' @param f (`formula`)\cr A formula from which covariance terms should be
#'   extracted.
#'
#' @return A list of covariance structure expressions found in `f`.
#'
#' @importFrom stats terms
#' @keywords internal
extract_covariance_terms <- function(f) {
  specials <- cov_types(c("abbr", "habbr"))
  terms <- stats::terms(f, specials = specials)
  covariance_terms <- Filter(Negate(is.null), attr(terms, "specials"))
  lapply(covariance_terms, function(i) {
    formula_rhs(formula(terms[i - (length(f) > 2)]))
  })
}

#' Drop Formula Terms used for Covariance Structure Definition
#'
#' @param f (`formula`)\cr A formula from which covariance terms should be
#'   dropped.
#'
#' @return The formula without accepted covariance terms.
#'
#' @importFrom stats terms drop.terms
#' @keywords internal
drop_covariance_terms <- function(f) {
  specials <- cov_types(c("abbr", "habbr"))

  terms <- stats::terms(f, specials = specials)
  covariance_terms <- Filter(Negate(is.null), attr(terms, "specials"))

  # if no covariance terms were found, return original formula
  if (length(covariance_terms) == 0) return(f)

  # drop covariance terms (position - 1 to account for response term)
  covariance_term_indices <- as.numeric(covariance_terms) - (length(f) > 2)

  terms <- terms[-covariance_term_indices]
  formula(terms)
}

#' Add Individual Covariance Variables As Terms to Formula
#'
#' @param f (`formula`)\cr A formula to which covariance structure terms should
#'   be added.
#' @param covariance (`cov_struct`)\cr A covariance structure object from which
#'   additional variables should be sourced.
#'
#' @return A new formula with included covariance terms.
#'
#' @keywords internal
add_covariance_variable_terms <- function(f, covariance) {
  cov_terms <- with(covariance, c(subject, visits, group))
  cov_terms <- paste(cov_terms, collapse = " + ")
  stats::update(f, stats::as.formula(paste(". ~ . + ", cov_terms)))
}
