#' `mmrm` Package
#'
#' `mmrm` implements mixed models with repeated measures (MMRM) in R.
#'
#' @aliases mmrm-package
"_PACKAGE"

#' @useDynLib mmrm, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import checkmate
#' @importFrom lifecycle deprecated
#' @importFrom Matrix .bdiag
#' @importFrom methods is
#' @importFrom stats acf
#' @importFrom stringr boundary
#' @importFrom parallel clusterApply
#' @importFrom Rdpack reprompt
#' @importFrom utils modifyList
#' @importFrom tibble as_tibble
#' @importFrom generics tidy glance augment
NULL
