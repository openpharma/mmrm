#' Obtain Cholesky for new data and theta.
#'
#' @description Use a fitted model to obtain (weighted) Cholesky factors
#' with new data, weights and theta.
#'
#' @param fit (`mmrm_fit`)\cr produced by [mmrm()].
#' @param data (`data.frame`)\cr input data frame.
#' @param weights (`numeric`)\cr vector of weights.
#' @param theta (`numeric`)\cr new theta estimates.
#' @param complete_case (`flag`)\cr whether to use complete input `data`.
#'
#' @return `list` of Cholesky factors.
#'
#' @keywords internal
h_get_chol <- function(fit, theta = NULL, data = NULL, weights = NULL, complete_case = TRUE) {
  assert_class(fit, "mmrm_fit")
  assert_data_frame(data, null.ok = TRUE)
  data <- h_default_value(data, fit$tmb_data$full_frame)
  assert_flag(complete_case)
  if (complete_case) {
    data <- data[complete.cases(data), ]
  }
  assert_numeric(weights, len = nrow(data), null.ok = TRUE)
  weights <- h_default_value(weights, rep(1, nrow(data)))
  assert_numeric(theta, len = length(fit$theta_est), null.ok = TRUE)
  theta <- h_default_value(theta, fit$theta_est)
  visit_vars <- fit$formula_parts$visit_var
  group_var <- fit$formula_parts$group_var
  subject_var <- fit$formula_parts$subject_var
  assert_names(names(data), must.include = c(visit_vars, subject_var, group_var))
  if (fit$formula_parts$is_spatial) {
    vn <- paste(vname(data), visit_vars, sep = "$")
    assert_data_frame(data[visit_vars], types = "numeric", .var.name = toString(vn))
  } else {
    vn <- paste(vname(data), visit_vars, sep = "$")
    data[[visit_vars]] <- h_factor_ref(data[[visit_vars]], fit$tmb_data$full_frame[[visit_vars]], vn)
  }
  if (!is.null(group_var)) {
    vn <- paste(vname(data), fit$formula_parts$group_var, sep = "$")
    data[[group_var]] <- h_factor_ref(data[[group_var]], fit$tmb_data$full_frame[[group_var]], vn)
  }
  tmb_data <- h_mmrm_tmb_data(
    h_mmrm_tmb_formula_parts(as.formula("rep(1, nrow(data))~1"), as.cov_struct(fit$formula_parts$formula)),
    data = data, weights, identical(fit$tmb_data$reml, 1L),
    accept_singular = TRUE, drop_visit_levels = FALSE
  )
  tmb_data$n_groups <- fit$tmb_data$n_groups
  tmb_data$subject_groups <- h_factor_ref(tmb_data$subject_groups, fit$tmb_data$full_frame[[group_var]])
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
