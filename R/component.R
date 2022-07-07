#' Component getter for \code{mmrm_tmb} objects
#'
#' @aliases component
#' @param x a \code{mmrm_tmb} object
#' @param name of the component to be retrieved
#' @param \dots ignored, for method compatibility
#'
#' @details Available `component()` names are as follows:
#' - 'call': low-level function call which generated the model.
#' - 'formula': model formula.
#' - 'dataset': data set name.
#' - 'cov_type': covariance structure type.
#' - 'n_theta': number of parameters.
#' - 'n_subjects': number of subjects.
#' - 'n_timepoints': number of modeled time points.
#' - 'n_obs': total number of observations.
#' - 'reml': was REML used (ML was used if \code{FALSE}).
#' - 'neg_log_lik': negative log likelihood.
#' - 'convergence': convergence code from optimizer.
#' - 'conv_message': message accompanying the convergence code.
#' - 'evaluations': number of function evaluations for optimization.
#' - 'vcov': estimated variance-covariance matrix of coefficients.
#' - 'varcor': estimated covariance matrix for residuals.
#' - 'theta_est': estimated variance parameters.
#' - 'beta_est': estimated coefficients.
#' - 'theta_vcov':  estimated variance-covariance matrix of variance parameters.
#' - 'x_matrix': design matrix used.
#' - 'y_vector': response vector used.
#' - 'jac_list': Jacobian, see  [h_jac_list()] for details.
#'
#' @seealso \code{\link[lme4]{getME.merMod}}
#'
#' @seealso \code{\link[glmmTMB]{getME.glmmTMB}}
#'
#' @export
component <- function(object,
                      name = c(
                        "cov_type", "n_theta", "n_subjects", "n_timepoints",
                        "n_obs", "vcov", "varcor", "formula", "dataset",
                        "reml", "convergence", "evaluations",
                        "conv_message", "call", "theta_est",
                        "beta_est", "x_matrix", "y_vector", "neg_log_lik",
                        "jac_list", "theta_vcov"
                      ),
                      ...) {
  assert_class(object, "mmrm_tmb")
  name <- match.arg(name, several.ok = TRUE)


  list_components <- sapply(
    X = name,
    FUN = switch,
    "call" = object$call,
    # Strings
    "cov_type" = object$formula_parts$cov_type,
    "formula" = deparse(object$call$formula),
    "dataset" = object$call$data,
    "reml" = object$reml,
    "conv_message" = object$opt_details$message,
    # Numeric of length 1
    "convergence" = object$opt_details$convergence,
    "neg_log_lik" = object$neg_log_lik,
    "n_theta" = length(object$theta_est),
    "n_subjects" = object$tmb_data$n_subjects,
    "n_timepoints" = object$tmb_data$n_visits,
    "n_obs" = length(object$tmb_data$y_vector),
    # Numeric of length > 1
    "evaluations" = unlist(ifelse(is.null(object$opt_details$evaluations),
                                  list(object$opt_details$counts),
                                  list(object$opt_details$evaluations)
    )),
    "beta_est" = object$beta_est,
    "theta_est" = object$theta_est,
    "y_vector" = object$tmb_data$y_vector,
    "jac_list" = object$jac_list,
    # Matrices
    "vcov" = object$beta_vcov,
    "varcor" = object$cov,
    "x_matrix" = object$tmb_data$x_matrix,
    "theta_vcov" = object$theta_vcov,
    # If not found
    "..foo.." =
      stop(sprintf(
        "component '%s' is not available for class \"%s\"",
        name, paste0(class(object), collapse = ", ")
      )), simplify = FALSE
  )

  if (length(name) == 1) list_components[[1]] else list_components
}

