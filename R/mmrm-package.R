#' `mmrm` Package
#'
#' `mmrm` implements mixed models with repeated measures (MMRM) in R.
#'
"_PACKAGE"

#' @useDynLib mmrm, .registration = TRUE
#'
#' @import checkmate
#' @importFrom lifecycle deprecated
#' @importFrom methods is
#' @importFrom stats acf
#' @importFrom parallel clusterApply
NULL
