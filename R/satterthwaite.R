#' Covariance Matrix for Coefficients Given Variance Parameters
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param model (`mmrm_tmb`)\cr initial model fit.
#'
#' @return Function with argument `theta` that calculates the covariance matrix
#'   for the coefficient vector `beta`.
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
#' model <- mmrm_tmb(formula, fev_data)
#' fun <- h_covbeta_fun(model)
#' fun(model$theta_est)
#' model$beta_vcov
h_covbeta_fun <- function(model) {
  assert_class(model, "mmrm_tmb")

  function(theta) {
    reported <- model$tmb_object$report(theta)
    reported$beta_vcov
  }
}

#' Formatting a Column from Jacobian Matrix as Matrix
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param jac_matrix (`matrix`)\cr full Jacobian matrix.
#' @param col (`int`)\cr column index.
#'
#' @return The column `col` of `jac_matrix` as a square matrix.
#'   Here the values in the column are used to fill the result column
#'   by column.
#' @export
#'
#' @examples
#' jac_matrix <- matrix(1:81, 9, 9)
#' h_jac_col_as_matrix(jac_matrix, 5)
h_jac_col_as_matrix <- function(jac_matrix, col) {
  assert_int(col)
  assert_matrix(jac_matrix, min.cols = col)
  p <- sqrt(nrow(jac_matrix))
  assert_integerish(p)

  jac_col <- jac_matrix[, col, drop = TRUE]
  matrix(jac_col, nrow = p, ncol = p)
}

#' Obtain List of Jacobian Matrix Entries for Covariance Matrix
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param covbeta_fun (`function`)\cr function calculating the covariance
#'   matrix of coefficients given variance parameters (`theta`), see
#'   [h_covbeta_fun()] to obtain this from a `mmrm_tmb` object.
#' @param theta_est (`numeric`)\cr variance parameters point estimate.
#'
#' @return List with one element per variance parameter containing a matrix
#'   of the same dimensions as the covariance matrix. The values are the derivatives
#'   with regards to this variance parameter.
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
#' model <- mmrm_tmb(formula, fev_data)
#' covbeta_fun <- h_covbeta_fun(model)
#' h_jac_list(covbeta_fun, model$theta_est)
h_jac_list <- function(covbeta_fun,
                       theta_est) {
  assert_function(covbeta_fun, args = "theta")
  assert_numeric(theta_est, any.missing = FALSE, min.len = 1L)

  jac_matrix <- numDeriv::jacobian(
    func = covbeta_fun,
    x = theta_est,
    method = "Richardson"
  )
  lapply(
    seq_len(ncol(jac_matrix)),
    FUN = h_jac_col_as_matrix,
    jac_matrix = jac_matrix
  )
}
