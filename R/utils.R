#' Extraction of Covariate Parts from Character Vector
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param covariates (`character`)\cr specification in the usual way, see examples.
#'
#' @return Character vector of the covariates involved in `covariates` specification.
#' @export
#'
#' @examples
#' h_get_covariate_parts(NULL)
#' h_get_covariate_parts("VISIT:ARM")
#' h_get_covariate_parts(c("VISIT:ARM", "COUNTRY"))
h_get_covariate_parts <- function(covariates) {
  assert_character(covariates, null.ok = TRUE)
  if (is.null(covariates)) {
    NULL
  } else {
    unique(unlist(strsplit(covariates, split = "\\*|:")))
  }
}
