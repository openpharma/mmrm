#' Drop Formula Terms used for Covariance Structure Definition
#'
#' @param f (`formula`)\cr
#'   A formula from which covariance terms should be dropped.
#'
#' @return The formula without accepted covariance terms
#'
#' @importFrom stats terms drop.terms
#' @keywords internal
drop_covariance_terms <- function(f) {
  specials <- Filter(Negate(is.na), COV_TYPES[, -3])

  terms <- stats::terms(f, specials = specials)
  covariance_term <- Filter(Negate(is.null), attr(terms, "specials"))

  if (length(covariance_term) == 0)
    return(f)

  terms <- stats::drop.terms(
    terms,
    as.numeric(covariance_term) - 1,
    keep.response = TRUE
  )

  formula(terms)
}



#' Add Individual Covariance Variables As Terms to Formula
#'
#' @param f (`formula`)\cr
#'   A formula to which covariance structure terms should be added
#' @param covariance (`cov_struct`)\cr
#'   A covariance structure object from which additional variables should be
#'   sourced.
#'
#' @return A new formula with included covariance terms
#'
#' @keywords internal
add_covariance_variable_terms <- function(f, covariance) {
  cov_terms <- with(covariance, c(subject, visits, group))
  cov_terms <- paste(cov_terms, collapse = " + ")
  stats::update(f, stats::as.formula(paste(". ~ . + ", cov_terms)))
}
