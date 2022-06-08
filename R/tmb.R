#' Control parameters for `TMB` fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param optimizer (`function`)\cr optimization function.
#' @param optimizer_args (`list`)\cr additional arguments to be passed to optimizer.
#' @param optimizer_control (`list`)\cr specific `control` argument for optimizer.
#'
#' @return List with the control parameters
#' @export
#'
#' @examples
#' h_mmrm_tmb_control(
#'   optimizer = stats::optim,
#'   optimizer_args = list(method = "L-BFGS-B")
#' )
h_mmrm_tmb_control <- function(optimizer = stats::nlminb,
                               optimizer_args = list(),
                               optimizer_control = list()) {
  assert_function(optimizer)
  assert_list(optimizer_args)
  assert_list(optimizer_control)

  structure(
    list(
      optimizer = optimizer,
      optimizer_args = optimizer_args,
      optimizer_control = optimizer_control
    ),
    class = "mmrm_tmb_control"
  )
}

#' Fitting an MMRM with `TMB`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula (`formula`)\cr model formula with exactly one special term
#'   specifying the visits within subjects, see details.
#' @param data (`data.frame`)\cr
#' @param start (`list` or `NULL`)\cr
#' @param control (`mmrm_tmb_control`)\cr
#'
#' @return
#'
#' @details The `formula` typically looks like:
#' `AVAL ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)`
#' so specifies response and covariates as usual, and exactly one special term
#' defines which correlation structure is used and what are the visit and
#' subject variables.
#'
#'
#' @export
#'
#' @examples
mmrm_tmb <- function(formula,
                     data,
                     start = NULL,
                     control = h_mmrm_tmb_control()) {
  assert_formula(formula)
  assert_data_frame(data)
  assert_list(start, null.ok = TRUE)
  assert_class(control, "mmrm_tmb_control")
}
