#' Building Model Formula
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param vars (`list`)\cr variables to use in the model
#' @param cor_struct (`character`)\cr speicify the covariance structure to use
#' @return formula for model
#' @export
#'
#' @examples
#' vars <- list( response = "AVAL", covariates = c("RACE", "SEX"),
#' id = "USUBJID", arm = "ARMCD", visit = "AVISIT")
#' h_build_formula(vars, "compound-symmetry")
#' h_build_formula(vars)
h_build_formula <- function(vars,
                            cor_struct = c(
                              "unstructured",
                              "toeplitz",
                              "auto-regressive",
                              "compound-symmetry"
                            )) {
  assert_list(vars)
  cor_struct <- match.arg(cor_struct)
  covariates_part <- paste(
    vars$covariates,
    collapse = " + "
  )
  arm_visit_part <- if (is.null(vars$arm)) {
    vars$visit
  } else {
    paste0(
      vars$arm,
      "*",
      vars$visit
    )
  }
  random_effects_fun <- switch(
    cor_struct,
    "unstructured" = "us",
    "toeplitz" = "toep",
    "auto-regressive" = "ar1",
    "compound-symmetry" = "cs"
  )
  random_effects_part <- paste0(
    random_effects_fun, "(0 + ", vars$visit, " | ", vars$id, ")"
  )
  rhs_formula <- paste(
    arm_visit_part,
    "+",
    random_effects_part
  )
  if (covariates_part != "") {
    rhs_formula <- paste(
      covariates_part,
      "+",
      rhs_formula
    )
  }
  stats::as.formula(paste(
    vars$response,
    "~",
    rhs_formula
  ))
}
