#' Component Access for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#' @param name (`character`)\cr the component(s) to be retrieved.
#' @return The corresponding component of the object, see details.
#'
#' @details Available `component()` names are as follows:
#' - `call`: low-level function call which generated the model.
#' - `formula`: model formula.
#' - `dataset`: data set name.
#' - `cov_type`: covariance structure type.
#' - `n_theta`: number of parameters.
#' - `n_subjects`: number of subjects.
#' - `n_timepoints`: number of modeled time points.
#' - `n_obs`: total number of observations.
#' - `reml`: was REML used (ML was used if `FALSE`).
#' - `neg_log_lik`: negative log likelihood.
#' - `convergence`: convergence code from optimizer.
#' - `conv_message`: message accompanying the convergence code.
#' - `evaluations`: number of function evaluations for optimization.
#' - `method`: Adjustment method which was used (for `mmrm` objects),
#'      otherwise `NULL` (for `mmrm_tmb` objects).
#' - `beta_vcov`: estimated variance-covariance matrix of coefficients
#'      (excluding aliased coefficients). For Kenward-Roger
#'      methods, the adjusted covariance matrix is returned (to still obtain the
#'      unadjusted covariance matrix use `object$beta_vcov`).
#' - `beta_vcov_complete`: estimated variance-covariance matrix including
#'      aliased coefficients with entries set to `NA`.
#' - `varcor`: estimated covariance matrix for residuals. If there are multiple
#'      groups, a named list of estimated covariance matrices for residuals will be
#'      returned. The names are the group levels.
#' - `theta_est`: estimated variance parameters.
#' - `beta_est`: estimated coefficients (excluding aliased coefficients).
#' - `beta_est_complete`: estimated coefficients including aliased coefficients
#'      set to `NA`.
#' - `beta_aliased`: whether each coefficient was aliased (i.e. cannot be estimated)
#'      or not.
#' - `theta_vcov`:  estimated variance-covariance matrix of variance parameters.
#' - `x_matrix`: design matrix used (excluding aliased columns).
#' - `y_vector`: response vector used.
#' - `jac_list`: Jacobian, see  [h_jac_list()] for details.
#'
#' @seealso In the `lme4` package there is a similar function `getME()`.
#'
#' @examples
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#' # Get all available components.
#' component(fit)
#' # Get convergence code and message.
#' component(fit, c("convergence", "conv_message"))
#' # Get modeled formula as a string.
#' component(fit, c("formula"))
#'
#' @export
component <- function(object,
                      name = c(
                        "cov_type", "n_theta", "n_subjects", "n_timepoints",
                        "n_obs", "beta_vcov", "beta_vcov_complete",
                        "varcor", "formula", "dataset", "n_groups",
                        "reml", "convergence", "evaluations", "method",
                        "conv_message", "call", "theta_est",
                        "beta_est", "beta_est_complete", "beta_aliased",
                        "x_matrix", "y_vector", "neg_log_lik", "jac_list", "theta_vcov"
                      )) {
  assert_class(object, "mmrm_tmb")
  name <- match.arg(name, several.ok = TRUE)

  list_components <- sapply(
    X = name,
    FUN = switch,
    "call" = object$call,
    # Strings.
    "cov_type" = object$formula_parts$cov_type,
    "formula" = deparse(object$call$formula),
    "dataset" = object$call$data,
    "reml" = object$reml,
    "conv_message" = object$opt_details$message,
    # Numeric of length 1.
    "convergence" = object$opt_details$convergence,
    "neg_log_lik" = object$neg_log_lik,
    "n_theta" = length(object$theta_est),
    "n_subjects" = object$tmb_data$n_subjects,
    "n_timepoints" = object$tmb_data$n_visits,
    "n_obs" = length(object$tmb_data$y_vector),
    "n_groups" = ifelse(is.list(object$cov), length(object$cov), 1L),
    # Numeric of length > 1.
    "evaluations" = unlist(ifelse(is.null(object$opt_details$evaluations),
      list(object$opt_details$counts),
      list(object$opt_details$evaluations)
    )),
    "method" = object$method,
    "beta_est" = object$beta_est,
    "beta_est_complete" =
      if (any(object$tmb_data$x_cols_aliased)) {
        stats::setNames(
          object$beta_est[names(object$tmb_data$x_cols_aliased)],
          names(object$tmb_data$x_cols_aliased)
        )
      } else {
        object$beta_est
      },
    "beta_aliased" = object$tmb_data$x_cols_aliased,
    "theta_est" = object$theta_est,
    "y_vector" = object$tmb_data$y_vector,
    "jac_list" = object$jac_list,
    # Matrices.
    "beta_vcov" =
      if (!is.null(object$method) && object$method %in% c("Kenward-Roger", "Kenward-Roger-Linear")) {
        object$beta_vcov_adj
      } else {
        object$beta_vcov
      },
    "beta_vcov_complete" =
      if (any(object$tmb_data$x_cols_aliased)) {
        stats::.vcov.aliased(
          aliased = object$tmb_data$x_cols_aliased,
          vc = component(object, "beta_vcov"),
          complete = TRUE
        )
      } else {
        object$beta_vcov
      },
    "varcor" = object$cov,
    "x_matrix" = object$tmb_data$x_matrix,
    "theta_vcov" = object$theta_vcov,
    # If not found.
    "..foo.." =
      stop(sprintf(
        "component '%s' is not available",
        name, paste0(class(object), collapse = ", ")
      )),
    simplify = FALSE
  )

  if (length(name) == 1) list_components[[1]] else list_components
}
