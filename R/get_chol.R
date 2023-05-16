#' Obtain Cholesky for new data and theta.
#'
#' @description Use a fitted model to obtain (weighted) Cholesky factors
#' with new data, weights and theta.
#'
#' @param fit (`mmrm_fit`)
#' @param data (`data.frame`) input data frame.
#' @param weights (`numeric`) vector of weights.
#' @param theta (`numeric`) new theta estimates.
#'
#' @return `list` of Cholesky factors.
#'
#' @export
get_chol <- function(fit, data, weights, theta) {
  assert_class(fit, "mmrm_fit")
  assert_data_frame(data)
  assert_numeric(weights, len = nrow(data), null.ok = TRUE)
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
  }
  assert_numeric(theta)
  visit_vars <- fit$formula_parts$visit_var
  checkmate::assert_names(names(data), must.include = visit_vars)
  if (fit$formula_parts$is_spatial) {
    vn <- paste(vname(data), visit_vars, sep = "$")
    checkmate::assert_data_frame(data[visit_vars], types = "numeric", .var.name = toString(vn))
  } else {
    checkmate::assert_string(visit_vars)
    fit_levels <- levels(fit$tmb_data$full_frame[[visit_vars]])
    vn <- paste(vname(data), visit_vars, sep = "$")
    if (is.character(data[[visit_vars]])) {
      checkmate::assert_subset(unique(data[[visit_vars]]), fit_levels, .var.name = vn)
      data[[visit_vars]] <- factor(data[[visit_vars]], levels = fit_levels)
    } else if (is.factor(data[[visit_vars]])) {
      checkmate::assert_factor(data[[visit_vars]], levels = fit_levels, .var.name = vn)
    } else {
      stop(vn, " must be of type character or factor!")
    }
  }
  if (!is.null(fit$formula_parts$group_var)) {
    vn <- paste(vname(data), fit$formula_parts$group_var, sep = "$")
    fit_levels <- levels(fit$tmb_data$full_frame[[fit$formula_parts$group_var]])
    checkmate::assert_factor(data[[fit$formula_parts$group_var]], levels = fit_levels, .var.name = vn)
  }
  tmb_data <- h_mmrm_tmb_data(
    fit$formula_parts, data = data, weights, identical(fit$tmb_data$reml, 1L),
    accept_singular = TRUE, drop_visit_levels = FALSE
  )
  tmb_parameters <- h_mmrm_tmb_parameters(fit$formula_parts, tmb_data, start = theta, n_groups = tmb_data$n_groups)
  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_object$report()$cholesky_all
}
