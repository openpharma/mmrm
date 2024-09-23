#' `mmrm` Package
#'
#' `mmrm` implements mixed models with repeated measures (MMRM) in R.
#'
#' @aliases mmrm-package
"_PACKAGE"

#' @useDynLib mmrm, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import checkmate
#' @importFrom methods is
#' @importFrom Rdpack reprompt
NULL

#' @importFrom generics tidy
#' @export
generics::tidy
#' @importFrom generics glance
#' @export
generics::glance
#' @importFrom generics augment
#' @export
generics::augment
