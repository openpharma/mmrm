#' Control Parameters for `TMB` Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param optimizer (`function`)\cr optimization function.
#' @param optimizer_args (`list`)\cr additional arguments to be passed to optimizer.
#' @param optimizer_control (`list`)\cr specific `control` argument for optimizer.
#'
#' @return List of class `mmrm_tmb_control` with the control parameters.
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

#' Processing the Formula for `TMB` Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula (`formula`)\cr original formula.
#'
#' @return List of class `mmrm_tmb_formula_parts` with elements:
#' - `formula`: the original input.
#' - `model_formula`: `formula` with the correlation term is removed.
#' - `full_formula`: same as `model_formula` but includes the `subject_var` and
#'      `visit_var`.
#' - `corr_type`: `string` with correlation term type (e.g. `"us"`).
#' - `visit_var`: `string` with the visit variable name.
#' - `subject_var`: `string` with the subject variable name.
#'
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' h_mmrm_tmb_formula_parts(formula)
h_mmrm_tmb_formula_parts <- function(formula) {
  assert_formula(formula)
  # Ensure that there is left and right hand side in the formula.
  assert_true(identical(length(formula), 3L))

  # Find the correlation specification term in the formula.
  corr_functions <- c("us", "toep", "ar1", "cs", "ad")
  terms_object <- stats::terms(formula, specials = corr_functions)
  found_specials <- attr(terms_object, "specials")
  corr_selected <- !sapply(found_specials, is.null)
  assert_true(identical(sum(corr_selected), 1L))
  corr_index <- found_specials[[which(corr_selected)]] + 1L # Subtract 1 for `list()`.
  terms_list <- attr(terms_object, "variables")
  corr_term <- terms_list[[corr_index]]

  # Remove the correlation term to obtain the model formula.
  model_formula <- stats::update(
    formula,
    stats::as.formula(paste(". ~ . -", deparse(corr_term)))
  )

  # Parse the correlation term to get correlation type, visit and subject variables.
  assert_true(identical(length(corr_term), 2L)) # 2 because `fun (...)`.
  corr_content <- corr_term[[2L]]
  assert_true(identical(length(corr_content), 3L)) # 3 because `y | x`.
  assert_true(identical(as.character(corr_content[[1L]]), "|"))
  visit_term <- corr_content[[2L]]
  subject_term <- corr_content[[3L]]
  assert_true(identical(length(visit_term), 1L))
  assert_true(identical(length(subject_term), 1L))
  subject_var <- deparse(subject_term)
  visit_var <- deparse(visit_term)
  full_formula <- stats::update(
    model_formula,
    stats::as.formula(paste(". ~ . +", subject_var, "+", visit_var))
  )
  structure(
    list(
      formula = formula,
      model_formula = model_formula,
      full_formula = full_formula,
      corr_type = deparse(corr_term[[1L]]),
      visit_var = visit_var,
      subject_var = subject_var
    ),
    class = "mmrm_tmb_formula_parts"
  )
}

#' Data for `TMB` Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula_parts (`mmrm_tmb_formula_parts`)\cr list with formula parts
#'   from [h_mmrm_tmb_formula_parts()].
#' @param data (`data.frame`)\cr which contains variables used in `formula_parts`.
#' @param reml (`flag`)\cr whether restricted maximum likelihood (REML) estimation is used,
#'   otherwise maximum likelihood (ML) is used.
#'
#' @return List of class `mmrm_tmb_data` with elements:
#' - `x_matrix`: `matrix` with `n` rows and `p` columns specifying the overall design matrix.
#' - `y_vector`: length `n` `numeric` specifying the overall response vector.
#' - `visits_zero_inds`: length `n` `integer` containing zero-based visits indices.
#' - `n_visits`: `int` with the number of visits, which is the dimension of the
#'      covariance matrix.
#' - `n_subjects`: `int` with the number of subjects.
#' - `subject_zero_inds`: length `n_subjects` `integer` containing the zero-based start
#'     indices for each subject.
#' - `subject_n_visits`: length `n_subjects` `integer` containing the number of
#'     observed visits for each subjects. So the sum of this vector equals `n`.
#' - `corr_type`: `int` specifying the correlation type.
#' - `reml`: `int` specifying whether REML estimation is used (1), otherwise ML (0).
#'
#' @details Note that the `subject_var` must not be factor but can also be character.
#'   If it is character, then it will be converted to factor internally. Here
#'   the levels will be the unique values, sorted alphabetically and numerically if there
#'   is a common string prefix of numbers in the character elements. For full control
#'   on the order please use a factor.
#'
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' formula_parts <- h_mmrm_tmb_formula_parts(formula)
#' tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = FALSE)
h_mmrm_tmb_data <- function(formula_parts,
                            data,
                            reml) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_data_frame(data)
  assert_names(
    names(data),
    must.include = c(formula_parts$visit_var, formula_parts$subject_var)
  )
  assert_factor(data[[formula_parts$visit_var]])
  assert_true(is.factor(data[[formula_parts$subject_var]]) || is.character(data[[formula_parts$subject_var]]))
  assert_flag(reml)

  if (is.character(data[[formula_parts$subject_var]])){
    data[[formula_parts$subject_var]] <- factor(
      data[[formula_parts$subject_var]],
      levels = stringr::str_sort(unique(data[[formula_parts$subject_var]]), numeric = TRUE)
    )
  }
  data <- data[order(data[[formula_parts$subject_var]], data[[formula_parts$visit_var]]), ]
  full_frame <- droplevels(stats::model.frame(formula_parts$full_formula, data = data))
  x_matrix <- stats::model.matrix(formula_parts$model_formula, data = full_frame)
  y_vector <- as.numeric(stats::model.response(full_frame))
  visits_zero_inds <- as.integer(full_frame[[formula_parts$visit_var]]) - 1L
  n_visits <- nlevels(full_frame[[formula_parts$visit_var]])
  n_subjects <- nlevels(full_frame[[formula_parts$subject_var]])
  subject_zero_inds <- which(!duplicated(full_frame[[formula_parts$subject_var]])) - 1L
  subject_n_visits <- c(utils::tail(subject_zero_inds, -1L), nrow(full_frame)) - subject_zero_inds
  assert_true(identical(subject_n_visits, as.integer(table(full_frame[[formula_parts$subject_var]]))))

  corr_type <- as.integer(switch(
    formula_parts$corr_type,
    us = 1,
    toep = 2,
    ar1 = 3,
    cs = 4,
    ad = 5
  ))

  structure(
    list(
      full_frame = full_frame,
      x_matrix = x_matrix,
      y_vector = y_vector,
      visits_zero_inds = visits_zero_inds,
      n_visits = n_visits,
      n_subjects = n_subjects,
      subject_zero_inds = subject_zero_inds,
      subject_n_visits = subject_n_visits,
      corr_type = corr_type,
      reml = as.integer(reml)
    ),
    class = "mmrm_tmb_data"
  )
}

#' Start Parameters for `TMB` Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula_parts (`mmrm_tmb_formula_parts`)\cr produced by
#'  [h_mmrm_tmb_formula_parts()].
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param start_values (`numeric` or `NULL`)\cr optional start values for variance
#'   parameters.
#'
#' @return List with element `theta` containing the start values for the variance
#'   parameters.
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' formula_parts <- h_mmrm_tmb_formula_parts(formula)
#' tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data, reml = FALSE)
#' pars <- h_mmrm_tmb_parameters(formula_parts, tmb_data, NULL)
h_mmrm_tmb_parameters <- function(formula_parts,
                                  tmb_data,
                                  start_values) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_class(tmb_data, "mmrm_tmb_data")

  n <- tmb_data$n_visits
  theta_dim <- as.integer(switch(formula_parts$corr_type,
    us = n * (n + 1) / 2,
    toep = 2 * n - 1,
    ar1 = 0,
    cs = 0,
    ad = 2 * n - 1
  ))
  if (!is.null(start_values)) {
    assert_numeric(start_values, len = theta_dim, any.missing = FALSE, finite = TRUE)
  } else {
    start_values <- rep(0, theta_dim)
  }
  list(theta = start_values)
}

#' Asserting Sane Start Values for `TMB` Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param tmb_object (`list`)\cr created with [TMB::MakeADFun()].
#'
#' @return Nothing, only used for assertions.
#' @export
h_mmrm_tmb_assert_start <- function(tmb_object) {
  assert_list(tmb_object)
  assert_subset(c("fn", "gr", "par"), names(tmb_object))

  if (is.na(tmb_object$fn(tmb_object$par))) {
    stop("negative log-likelihood is NaN at starting parameter values")
  }
  if (any(is.na(tmb_object$gr(tmb_object$par)))) {
    stop("some elements of gradient are NaN at starting parameter values")
  }
}

#' Asserting and Checking `TMB` Optimization Result
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param tmb_object (`list`)\cr created with [TMB::MakeADFun()].
#' @param tmb_opt (`list`)\cr optimization result.
#'
#' @return Nothing, only used to generate messages, warnings or errors.
#' @export
h_mmrm_tmb_assert_opt <- function(tmb_object,
                                  tmb_opt) {
  assert_list(tmb_object)
  assert_subset(c("fn", "gr", "par", "he"), names(tmb_object))
  assert_list(tmb_opt)
  assert_subset(c("par", "objective", "convergence", "message"), names(tmb_opt))

  if (!is.null(tmb_opt$convergence) && tmb_opt$convergence != 0) {
    warning("Model convergence problem: ", tmb_opt$message, ".")
  } else {
    tmb_hessian <- tmb_object$he(tmb_opt$par)
    eigen_vals <- try(
      eigen(tmb_hessian, symmetric = TRUE)$values,
      silent = TRUE
    )
    if (is(eigen_vals, "try-error")) {
      stop("Model convergence problem: Cannot calculate hessian eigenvalues")
    } else if (min(eigen_vals) < .Machine$double.eps) {
      warning(
        "Model convergence problem: ",
        "hessian has negative or very small eigenvalues"
      )
    }
  }
}

#' Build `TMB` Fit Result List
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper does some simple post-processing of the `TMB` object and
#' optimization results, including setting names, inverting matrices etc.
#'
#' @param tmb_object (`list`)\cr created with [TMB::MakeADFun()].
#' @param tmb_opt (`list`)\cr optimization result.
#' @param data (`data.frame`)\cr input data containing the variables used
#'   in `formula`.
#' @param formula_parts (`mmrm_tmb_formula_parts`)\cr produced by
#'  [h_mmrm_tmb_formula_parts()].
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#'
#' @return List pf class `mmrm_tmb` with:
#'   - `cov`: estimated covariance matrix.
#'   - `beta_est`: vector of coefficient estimates.
#'   - `beta_vcov`: Variance-covariance matrix for coefficient estimates.
#'   - `theta_est`: vector of variance parameter estimates.
#'   - `theta_vcov`: variance-covariance matrix for variance parameter estimates.
#'   - `neg_log_lik`: obtained negative log-likelihood.
#'   - `formula_parts`: input.
#'   - `data`: input.
#'   - `reml`: input as a flag.
#'   - `opt_details`: list with optimization details including convergence code.
#'   - `tmb_object`: original `TMB` object created with [TMB::MakeADFun()].
#'   - `tmb_data`: input.
#' @export
h_mmrm_tmb_fit <- function(tmb_object,
                           tmb_opt,
                           data,
                           formula_parts,
                           tmb_data) {
  assert_list(tmb_object)
  assert_subset(c("fn", "gr", "par", "he"), names(tmb_object))
  assert_list(tmb_opt)
  assert_subset(c("par", "objective", "convergence", "message"), names(tmb_opt))
  assert_data_frame(data)
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_class(tmb_data, "mmrm_tmb_data")

  tmb_report <- tmb_object$report(par = tmb_opt$par)
  x_matrix_cols <- colnames(tmb_data$x_matrix)
  visit_names <- levels(data[[formula_parts$visit_var]])
  cov <- tcrossprod(tmb_report$covariance_lower_chol)
  dimnames(cov) <- list(visit_names, visit_names)
  beta_est <- tmb_report$beta
  names(beta_est) <- x_matrix_cols
  beta_vcov <- tmb_report$beta_vcov
  dimnames(beta_vcov) <- list(x_matrix_cols, x_matrix_cols)
  theta_est <- tmb_opt$par
  names(theta_est) <- NULL
  theta_vcov <- solve(tmb_object$he(tmb_opt$par))
  opt_details_names <- setdiff(
    names(tmb_opt),
    c("par", "objective")
  )

  structure(
    list(
      cov = cov,
      beta_est = beta_est,
      beta_vcov = beta_vcov,
      theta_est = theta_est,
      theta_vcov = theta_vcov,
      neg_log_lik = tmb_opt$objective,
      formula_parts = formula_parts,
      data = data,
      reml = as.logical(tmb_data$reml),
      opt_details = tmb_opt[opt_details_names],
      tmb_object = tmb_object,
      tmb_data = tmb_data
    ),
    class = "mmrm_tmb"
  )
}

#' Fitting an MMRM with `TMB`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is the low-level function to fit an MMRM. Note that this does not
#' try different optimizers or adds Jacobian information etc. in contrast to
#' [mmrm()].
#'
#' @param formula (`formula`)\cr model formula with exactly one special term
#'   specifying the visits within subjects, see details.
#' @param data (`data.frame`)\cr input data containing the variables used
#'   in `formula`.
#' @inheritParams h_mmrm_tmb_data
#' @param start (`numeric` or `NULL`)\cr optional start values for variance
#'   parameters.
#' @param control (`mmrm_tmb_control`)\cr list of control options produced
#'   by [h_mmrm_tmb_control()].
#'
#' @return List of class `mmrm_tmb`, see [h_mmrm_tmb_fit()] for details.
#'
#' @details The `formula` typically looks like:
#' `FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)`
#' so specifies response and covariates as usual, and exactly one special term
#' defines which correlation structure is used and what are the visit and
#' subject variables.
#'
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' data <- fev_data
#' system.time(result <- h_mmrm_tmb(formula, data))
h_mmrm_tmb <- function(formula,
                       data,
                       reml = TRUE,
                       start = NULL,
                       control = h_mmrm_tmb_control()) {
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, data, reml)
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start)
  assert_class(control, "mmrm_tmb_control")

  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  h_mmrm_tmb_assert_start(tmb_object)
  tmb_opt <- with(
    tmb_object,
    do.call(
      what = control$optimizer,
      args = c(
        list(par, fn, gr, control = control$optimizer_control),
        control$optimizer_args
      )
    )
  )
  # Ensure negative log likelihood is stored in `objective` element of list.
  if ("value" %in% names(tmb_opt)) {
    tmb_opt$objective <- tmb_opt$value
    tmb_opt$value <- NULL
  }
  h_mmrm_tmb_assert_opt(tmb_object, tmb_opt)
  h_mmrm_tmb_fit(tmb_object, tmb_opt, data, formula_parts, tmb_data)
}
