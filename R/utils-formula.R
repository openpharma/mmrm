#' Extract Formula Terms used for Covariance Structure Definition
#'
#' @param f (`formula`)\cr a formula from which covariance terms should be
#'   extracted.
#'
#' @return A list of covariance structure expressions found in `f`.
#'
#' @importFrom stats terms
#' @keywords internal
h_extract_covariance_terms <- function(f) {
  specials <- cov_types(c("abbr", "habbr"))
  terms <- stats::terms(formula_rhs(f), specials = specials)
  covariance_terms <- Filter(length, attr(terms, "specials"))
  variables <- attr(terms, "variables")
  lapply(covariance_terms, function(i) variables[[i + 1]])
}

#' Drop Formula Terms used for Covariance Structure Definition
#'
#' @param f (`formula`)\cr a formula from which covariance terms should be
#'   dropped.
#'
#' @return The formula without accepted covariance terms.
#'
#' @details `terms` is used and it will preserve the environment attribute.
#' This ensures the returned formula and the input formula have the same environment.
#' @importFrom stats terms drop.terms
#' @keywords internal
h_drop_covariance_terms <- function(f) {
  specials <- cov_types(c("abbr", "habbr"))

  terms <- stats::terms(f, specials = specials)
  covariance_terms <- Filter(Negate(is.null), attr(terms, "specials"))

  # if no covariance terms were found, return original formula
  if (length(covariance_terms) == 0) {
    return(f)
  }

  # drop covariance terms (position - 1 to account for response term)
  covariance_term_indices <- as.numeric(covariance_terms) - (length(f) > 2)

  terms <- terms[-covariance_term_indices]
  formula(terms)
}

#' Add Individual Covariance Variables As Terms to Formula
#'
#' @param f (`formula`)\cr a formula to which covariance structure terms should
#'   be added.
#' @param covariance (`cov_struct`)\cr a covariance structure object from which
#'   additional variables should be sourced.
#'
#' @return A new formula with included covariance terms.
#'
#' @details [stats::update()] is used to append the covariance structure and the environment
#' attribute will not be changed. This ensures the returned formula and the input formula
#' have the same environment.
#'
#' @keywords internal
h_add_covariance_terms <- function(f, covariance) {
  cov_terms <- with(covariance, c(subject, visits, group))
  cov_terms <- paste(cov_terms, collapse = " + ")
  stats::update(f, stats::as.formula(paste(". ~ . + ", cov_terms)))
}

#' Drop Formula Terms with Character
#'
#' Drop formula terms from the original formula with character representation.
#'
#' @param f (`formula`)\cr a formula to be updated.
#' @param drops (`character`)\cr representation of elements to be removed.
#'
#' @details Elements in `drops` will be removed from the formula, while the environment
#' of the formula is unchanged. If `drops` is `NULL` or `character(0)`, the formula is
#' unchanged.
#' @return A new formula with elements in `drops` removed.
#'
#' @keywords internal
h_drop_terms <- function(f, drops) {
  assert_character(drops, null.ok = TRUE)
  if (length(drops) == 0L) {
    return(f)
  }
  drop_terms <- stats::as.formula(sprintf(". ~ . - %s", paste(drops, collapse = " - ")))
  assert_subset(setdiff(all.vars(drop_terms), "."), all.vars(f))
  stats::update(f, drop_terms)
}
