#' Control Parameters for `TMB` Fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param optimizer (`function`)\cr optimization function.
#' @param optimizer_args (`list`)\cr additional arguments to be passed to optimizer.
#' @param optimizer_control (`list`)\cr specific `control` argument for optimizer.
#' @param start (`numeric` or `NULL`)\cr optional start values for variance
#'   parameters.
#' @param accept_singular (`flag`)\cr whether singular design matrices are reduced
#'   to full rank automatically and additional coefficient estimates will be missing.
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
                               optimizer_control = list(),
                               start = NULL,
                               accept_singular = TRUE) {
  assert_function(optimizer)
  assert_list(optimizer_args)
  assert_list(optimizer_control)
  assert_numeric(start, null.ok = TRUE)
  assert_flag(accept_singular)

  structure(
    list(
      optimizer = optimizer,
      optimizer_args = optimizer_args,
      optimizer_control = optimizer_control,
      start = start,
      accept_singular = accept_singular
    ),
    class = "mmrm_tmb_control"
  )
}

#' Processing the covariance term
#'
#' @param cov_term (`call`)\cr covariance term.
#'
#' @return Named list of with elements:
#' - `visit_var`: `string` with the visit variable name.
#' - `subject_var`: `string` with the subject variable name.
#' - `group_var`: `string` with the group variable name. If no group specified, this element
#'      is NULL.
#' @keywords internal
h_mmrm_tmb_extract_vars <- function(cov_term) {
  assert_true(identical(length(cov_term), 2L)) # 2 because `fun (...)`.
  cov_content <- cov_term[[2L]]
  if (!identical(length(cov_content), 3L) || !identical(as.character(cov_content[[1L]]), "|")) {
    stop(
      "Covariance structure must be of the form `",
      cov_term[[1]],
      "(time|(group/)subject)`"
    )
  }
  visit_term <- cov_content[[2L]]
  if (!identical(length(visit_term), 1L)) {
    stop(
      "`time` in `(time|(group/)subject)` must be specified as one single variable."
    )
  }
  if (identical(length(cov_content[[3L]]), 1L)) {
    subject_term <- cov_content[[3L]]
    group_var <- NULL
  } else if (identical(length(cov_content[[3L]]), 3L) && identical(as.character(cov_content[[3L]][[1L]]), "/")) {
    subject_term <- cov_content[[3L]][[3L]]
    group_term <- cov_content[[3L]][[2L]]
    if (!identical(length(group_term), 1L)) {
      stop(
        "`group` in `(time|group/subject)` must be specified as one single variable."
      )
    }
    group_var <- deparse(group_term)
  } else {
    stop(
      "Covariance structure with grouping must be of the form `",
      cov_term[[1]],
      "(time|group/subject)`"
    )
  }
  if (!identical(length(subject_term), 1L)) {
    stop(
      "`subject` in `(time|(group/)subject)` must be specified as one single variable."
    )
  }
  subject_var <- deparse(subject_term)
  visit_var <- deparse(visit_term)
  return(
    list(subject_var = subject_var, visit_var = visit_var, group_var = group_var)
  )
}

#' Processing the Formula for `TMB` Fit
#'
#' @param formula (`formula`)\cr original formula.
#'
#' @return List of class `mmrm_tmb_formula_parts` with elements:
#' - `formula`: the original input.
#' - `model_formula`: `formula` with the covariance term is removed.
#' - `full_formula`: same as `model_formula` but includes the `subject_var` and
#'      `visit_var`.
#' - `cov_type`: `string` with covariance term type (e.g. `"us"`).
#' - `visit_var`: `string` with the visit variable name.
#' - `subject_var`: `string` with the subject variable name.
#' - `group_var`: `string` with the group variable name. If no group specified, this element
#'      is NULL.
#' @keywords internal
h_mmrm_tmb_formula_parts <- function(formula) {
  assert_formula(formula)
  # Ensure that there is left and right hand side in the formula.
  assert_true(identical(length(formula), 3L))

  # Find the covariance specification term in the formula.
  cov_functions <- c(cov_type, cov_type_spatial)
  terms_object <- stats::terms(formula, specials = cov_functions)
  found_specials <- attr(terms_object, "specials")
  cov_selected <- Filter(Negate(is.null), found_specials)
  if (length(cov_selected) == 0) {
    stop(
      "Covariance structure must be specified in formula. ",
      "Possible covariance structures include: ",
      toString(cov_functions)
    )
  } else if (length(cov_selected) > 1) {
    stop(
      "Only one covariance structure can be specified. ",
      "Currently specified covariance structures are: ",
      toString(names(cov_selected))
    )
  }
  terms_list <- attr(terms_object, "variables")
  cov_term <- terms_list[[unlist(cov_selected) + 1L]]
  # Remove the covariance term to obtain the model formula.
  model_formula <- stats::update(
    formula,
    stats::as.formula(paste(". ~ . -", deparse(cov_term)))
  )
  cov_vars <- h_mmrm_tmb_extract_vars(cov_term)
  full_formula <- stats::update(
    model_formula,
    stats::as.formula(paste(". ~ . +", cov_vars$subject_var, "+", cov_vars$visit_var))
  )
  if (!is.null(cov_vars$group_var)) {
    full_formula <- stats::update(
      full_formula,
      stats::as.formula(paste(". ~ . +", cov_vars$group_var))
    )
  }
  structure(
    .Data = list(
      formula = formula,
      model_formula = model_formula,
      full_formula = full_formula,
      cov_type = deparse(cov_term[[1L]]),
      visit_var = cov_vars$visit_var,
      subject_var = cov_vars$subject_var,
      group_var = cov_vars$group_var
    ),
    class = "mmrm_tmb_formula_parts"
  )
}

#' Data for `TMB` Fit
#'
#' @param formula_parts (`mmrm_tmb_formula_parts`)\cr list with formula parts
#'   from [h_mmrm_tmb_formula_parts()].
#' @param data (`data.frame`)\cr which contains variables used in `formula_parts`.
#' @param reml (`flag`)\cr whether restricted maximum likelihood (REML) estimation is used,
#'   otherwise maximum likelihood (ML) is used.
#' @param accept_singular (`flag`)\cr whether below full rank design matrices are reduced
#'   to full rank `x_matrix` and remaining coefficients will be missing as per
#'   `x_cols_aliased`. Otherwise the function fails for rank deficient design matrices.
#'
#' @return List of class `mmrm_tmb_data` with elements:
#' - `full_frame`: `data.frame` with `n` rows containing all variables needed in the model.
#' - `x_matrix`: `matrix` with `n` rows and `p` columns specifying the overall design matrix.
#' - `x_cols_aliased`: `logical` with potentially more than `p` elements indicating which
#'      columns in the original design matrix have been left out to obtain a full rank
#'      `x_matrix`.
#' - `y_vector`: length `n` `numeric` specifying the overall response vector.
#' - `visits_zero_inds`: length `n` `integer` containing zero-based visits indices.
#' - `n_visits`: `int` with the number of visits, which is the dimension of the
#'      covariance matrix.
#' - `n_subjects`: `int` with the number of subjects.
#' - `subject_zero_inds`: length `n_subjects` `integer` containing the zero-based start
#'     indices for each subject.
#' - `subject_n_visits`: length `n_subjects` `integer` containing the number of
#'     observed visits for each subjects. So the sum of this vector equals `n`.
#' - `cov_type`: `string` value specifying the covariance type.
#' - `reml`: `int` specifying whether REML estimation is used (1), otherwise ML (0).
#' - `subject_groups`: `factor` specifying the grouping for each subject.
#' - `n_groups`: `int` with the number of total groups
#'
#' @details Note that the `subject_var` must not be factor but can also be character.
#'   If it is character, then it will be converted to factor internally. Here
#'   the levels will be the unique values, sorted alphabetically and numerically if there
#'   is a common string prefix of numbers in the character elements. For full control
#'   on the order please use a factor.
#'
#' @keywords internal
h_mmrm_tmb_data <- function(formula_parts,
                            data,
                            reml,
                            accept_singular) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_data_frame(data)
  varname <- formula_parts[grepl("_var", names(formula_parts))]
  assert_names(
    names(data),
    must.include = unlist(varname, use.names = FALSE)
  )
  assert_true(is.factor(data[[formula_parts$subject_var]]) || is.character(data[[formula_parts$subject_var]]))
  assert_flag(reml)
  assert_flag(accept_singular)

  if (is.character(data[[formula_parts$subject_var]])) {
    data[[formula_parts$subject_var]] <- factor(
      data[[formula_parts$subject_var]],
      levels = stringr::str_sort(unique(data[[formula_parts$subject_var]]), numeric = TRUE)
    )
  }
  data <- data[order(data[[formula_parts$subject_var]], data[[formula_parts$visit_var]]), ]
  full_frame <- droplevels(stats::model.frame(formula_parts$full_formula, data = data))

  x_matrix <- stats::model.matrix(formula_parts$model_formula, data = full_frame)
  x_cols_aliased <- stats::setNames(rep(FALSE, ncol(x_matrix)), nm = colnames(x_matrix))
  qr_x_mat <- qr(x_matrix)
  if (qr_x_mat$rank < ncol(x_matrix)) {
    cols_to_drop <- utils::tail(qr_x_mat$pivot, ncol(x_matrix) - qr_x_mat$rank)
    if (!accept_singular) {
      stop(
        "design matrix only has rank ", qr_x_mat$rank, " and ", length(cols_to_drop),
        " columns (", toString(colnames(x_matrix)[cols_to_drop]), ") could be dropped",
        " to achieve full rank ", ncol(x_matrix), " by using `accept_singular = TRUE`"
      )
    }
    assign_attr <- attr(x_matrix, "assign")
    contrasts_attr <- attr(x_matrix, "contrasts")
    x_matrix <- x_matrix[, -cols_to_drop, drop = FALSE]
    x_cols_aliased[cols_to_drop] <- TRUE
    attr(x_matrix, "assign") <- assign_attr[-cols_to_drop]
    attr(x_matrix, "contrasts") <- contrasts_attr
  }

  y_vector <- as.numeric(stats::model.response(full_frame))
  n_subjects <- nlevels(full_frame[[formula_parts$subject_var]])
  subject_zero_inds <- which(!duplicated(full_frame[[formula_parts$subject_var]])) - 1L
  subject_n_visits <- c(utils::tail(subject_zero_inds, -1L), nrow(full_frame)) - subject_zero_inds
  assert_true(identical(subject_n_visits, as.integer(table(full_frame[[formula_parts$subject_var]]))))
  if (!is.null(formula_parts$group_var)) {
    assert_factor(data[[formula_parts$group_var]])
    subject_groups <- full_frame[[formula_parts$group_var]][subject_zero_inds + 1L]
    n_groups <- nlevels(subject_groups)
  } else {
    subject_groups <- factor(rep(0L, n_subjects))
    n_groups <- 1L
  }
  coordinates <- full_frame[formula_parts$visit_var]
  if (formula_parts$cov_type %in% cov_type_spatial) {
    lapply(
      coordinates,
      assert_numeric
    )
    coordinates_matrix <- as.matrix(coordinates)
    visits_zero_inds <- 0L
    n_visits <- 2L
  } else {
    assert(identical(ncol(coordinates), 1L))
    assert_factor(coordinates[[1L]])
    visits_zero_inds <- as.integer(coordinates[[1L]]) - 1L
    coordinates_matrix <- as.matrix(visits_zero_inds, ncol = 1)
    n_visits <- nlevels(coordinates[[1L]])
  }
  ret <- structure(
    list(
      full_frame = full_frame,
      x_matrix = x_matrix,
      x_cols_aliased = x_cols_aliased,
      coordinates = coordinates_matrix,
      y_vector = y_vector,
      visits_zero_inds = visits_zero_inds,
      n_visits = n_visits,
      n_subjects = n_subjects,
      subject_zero_inds = subject_zero_inds,
      subject_n_visits = subject_n_visits,
      cov_type = formula_parts$cov_type,
      reml = as.integer(reml),
      subject_groups = subject_groups,
      n_groups = n_groups
    ),
    class = "mmrm_tmb_data"
  )
  
}

#' Start Parameters for `TMB` Fit
#'
#' @param formula_parts (`mmrm_tmb_formula_parts`)\cr produced by
#'  [h_mmrm_tmb_formula_parts()].
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param start (`numeric` or `NULL`)\cr optional start values for variance
#'   parameters.
#' @param n_groups (`int`)\cr number of groups.
#' @return List with element `theta` containing the start values for the variance
#'   parameters.
#'
#' @keywords internal
h_mmrm_tmb_parameters <- function(formula_parts,
                                  tmb_data,
                                  start,
                                  n_groups = 1L) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_class(tmb_data, "mmrm_tmb_data")

  m <- tmb_data$n_visits
  theta_dim <- as.integer(switch(formula_parts$cov_type,
    us = m * (m + 1) / 2,
    toep = m,
    toeph = 2 * m - 1,
    ar1 = 2,
    ar1h = m + 1,
    ad = m,
    adh = 2 * m - 1,
    cs = 2,
    csh = m + 1,
    gp_exp = 2
  ))
  theta_dim <- theta_dim * n_groups
  if (!is.null(start)) {
    assert_numeric(start, len = theta_dim, any.missing = FALSE, finite = TRUE)
  } else {
    start <- rep(0, theta_dim)
  }
  list(theta = start)
}

#' Asserting Sane Start Values for `TMB` Fit
#'
#' @param tmb_object (`list`)\cr created with [TMB::MakeADFun()].
#'
#' @return Nothing, only used for assertions.
#'
#' @keywords internal
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
#' @param tmb_object (`list`)\cr created with [TMB::MakeADFun()].
#' @param tmb_opt (`list`)\cr optimization result.
#'
#' @return Nothing, only used to generate messages, warnings or errors.
#'
#' @keywords internal
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

#' Extract covariance matrix from `TMB` report and tmb data
#'
#' This helper does some simple post-processing to extract covariance matrix or named
#' list of covariance matrices if the fitting is using grouped covariance matrices.
#'
#' @param tmb_report (`list`)\cr report created with [TMB::MakeADFun()] report function.
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param visit_var `character`\cr character vector of length 1 of the visit variable
#' @return Return a simple covariance matrix if there is no grouping, or a named list of estimated grouped covariance matrices,
#' with its name equal to the group levels.
#'
#' @keywords internal
h_mmrm_tmb_extract_cov <- function(tmb_report, tmb_data, visit_var) {
  d <- dim(tmb_report$covariance_lower_chol)
  visits <- tmb_data$full_frame[[visit_var]]
  if (is.factor(visits)) {
    visit_names <- levels(visits)
  } else {
    visit_names <- c(0, 1)
  }
  cov <- lapply(
    seq_len(d[1] / d[2]),
    function(i) {
      ret <- tcrossprod(tmb_report$covariance_lower_chol[seq(1 + (i - 1) * d[2], i * d[2]), ])
      dimnames(ret) <- list(visit_names, visit_names)
      return(ret)
    }
  )
  if (identical(tmb_data$n_groups, 1L)) {
    cov <- cov[[1]]
  } else {
    names(cov) <- levels(tmb_data$subject_groups)
  }
  return(cov)
}

#' Build `TMB` Fit Result List
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
#' @return List of class `mmrm_tmb` with:
#'   - `cov`: estimated covariance matrix, or named list of estimated group specific covariance matrices.
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
#'
#' @keywords internal
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
  cov <- h_mmrm_tmb_extract_cov(tmb_report, tmb_data, formula_parts$visit_var)
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
#' @param control (`mmrm_tmb_control`)\cr list of control options produced
#'   by [h_mmrm_tmb_control()].
#'
#' @return List of class `mmrm_tmb`, see [h_mmrm_tmb_fit()] for details.
#'
#' @details The `formula` typically looks like:
#' `FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)`
#' so specifies response and covariates as usual, and exactly one special term
#' defines which covariance structure is used and what are the visit and
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
                       control = h_mmrm_tmb_control()) {
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  assert_class(control, "mmrm_tmb_control")
  tmb_data <- h_mmrm_tmb_data(formula_parts, data, reml, accept_singular = control$accept_singular)
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = control$start, n_groups = tmb_data$n_groups)
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
  fit <- h_mmrm_tmb_fit(tmb_object, tmb_opt, data, formula_parts, tmb_data)

  fun_call <- match.call()
  fun_call$formula <- eval(formula_parts$formula)
  fun_call$data <- ifelse(
    !is.null(attr(data, which = "dataname")),
    attr(data, which = "dataname"),
    toString(match.call()$data)
  )
  fit$call <- fun_call
  fit
}
