#' Extract group/subject vars from covariance term
#'
#' @details a covariance term, which takes the following form:
#' `us(time | group / subject)`, or `sp_exp(time1, time2, ... | group / subject)`.
#' Only for spatial covariance structure can have multiple time coordinates.
#' This function, take the call of `time | group / subject`, last element of the covariance term,
#' extract the corresponding grouping var, time var and subject var.
#'
#' @param cov_content (`call`)\cr covariance term.
#'
#' @return Named list with elements:
#' - `visit_var`: `character` with the visit variable name.
#' - `subject_var`: `string` with the subject variable name.
#' - `group_var`: `string` with the group variable name. If no group specified, this element
#'      is NULL.
#' @keywords internal
h_mmrm_tmb_extract_terms <- function(cov_content) {
  if (!identical(length(cov_content), 3L) || !identical(as.character(cov_content[[1L]]), "|")) {
    stop(
      "Covariance structure must be of the form `time|(group/)subject`."
    )
  }
  visit_term <- cov_content[[2L]]
  if (!identical(length(visit_term), 1L)) {
    stop(
      "`time` in `time|(group/)subject` must be specified as one single variable."
    )
  } else if (identical(length(visit_term), 1L)) {
    visit_var <- as.character(visit_term)
  }
  if (identical(length(cov_content[[3L]]), 1L)) {
    subject_term <- cov_content[[3L]]
    group_var <- NULL
  } else if (identical(length(cov_content[[3L]]), 3L) && identical(as.character(cov_content[[3L]][[1L]]), "/")) {
    subject_term <- cov_content[[3L]][[3L]]
    group_term <- cov_content[[3L]][[2L]]
    if (!identical(length(group_term), 1L)) {
      stop(
        "`group` in `time|group/subject` must be specified as one single variable."
      )
    }
    group_var <- deparse(group_term)
  } else {
    stop(
      "Covariance structure must be of the form `time|(group/)subject`."
    )
  }
  if (!identical(length(subject_term), 1L)) {
    stop(
      "`subject` in `time|(group/)subject` must be specified as one single variable."
    )
  }
  subject_var <- deparse(subject_term)
  return(
    list(subject_var = subject_var, visit_var = visit_var, group_var = group_var)
  )
}

#' Processing the covariance term
#'
#' @param cov_term (`call`)\cr covariance term.
#'
#' @return Named list of with elements:
#' - `visit_var`: `character` with the visit variable name.
#' - `subject_var`: `string` with the subject variable name.
#' - `group_var`: `string` with the group variable name. If no group specified, this element
#'      is NULL.
#' - `is_spatial`: `logical` indicator of whether the covariance structure is spatial
#' @keywords internal
h_mmrm_tmb_extract_vars <- function(cov_term) {
  assert_true(length(cov_term) >= 2L) # 2 because `fun (...)`.
  is_spatial <- as.character(cov_term[[1]]) %in% cov_type_spatial
  cov_content <- cov_term[[length(cov_term)]] # this content is time | group / subject, last element
  cov_terms <- h_mmrm_tmb_extract_terms(cov_content)
  cov_terms$is_spatial <- is_spatial
  if (is_spatial) {
    # spatial can have sp_exp(time1, ... | group / subject)
    visit_terms <- cov_term[c(-1L, -length(cov_term))]
    term_lengths <- vapply(
      visit_terms,
      length,
      1L
    )
    if (any(term_lengths != 1L)) {
      stop(
        "Spatial covariance term should have all `time` variables as single."
      )
    }
    visit_vars <- as.character(visit_terms)
    cov_terms$visit_var <- c(visit_vars, cov_terms$visit_var)
    if (any(duplicated(cov_terms$visit_var))) {
      warning(
        "Duplicated `time` variable spotted: ",
        toString(cov_terms$visit_var[unique(duplicated(cov_terms$visit_var))]),
        ". This may indicate input errors in the formula."
      )
    }
  } else {
    if (length(cov_term) > 2L) {
      stop(
        "Non-spatial covariance term should not include multiple `time` variables."
      )
    }
  }
  return(cov_terms)
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
#' - `is_spatial`: `flag` indicator of whether the covariance structure is spatial
#' - `visit_var`: `character` with the visit variable name.
#' - `subject_var`: `string` with the subject variable name.
#' - `group_var`: `string` with the group variable name. If no group specified, this element
#'      is `NULL`.
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
    stats::as.formula(paste(". ~ . +", cov_vars$subject_var, "+", paste0(cov_vars$visit_var, collapse = "+")))
  )
  if (!is.null(cov_vars$group_var)) {
    full_formula <- stats::update(
      full_formula,
      stats::as.formula(paste(". ~ . +", cov_vars$group_var))
    )
  }
  cov_term_type <- deparse(cov_term[[1L]])
  if (length(cov_vars$visit_var) > 1L && !cov_term_type %in% cov_type_spatial) {
    stop(
      "Only spatial covariance support multiple coordinates"
    )
  }
  structure(
    list(
      formula = formula,
      model_formula = model_formula,
      full_formula = full_formula,
      cov_type = cov_term_type,
      is_spatial = cov_vars$is_spatial,
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
#' @param weights (`vector`)\cr weights to be used in the fitting process.
#' @param reml (`flag`)\cr whether restricted maximum likelihood (REML) estimation is used,
#'   otherwise maximum likelihood (ML) is used.
#' @param accept_singular (`flag`)\cr whether below full rank design matrices are reduced
#'   to full rank `x_matrix` and remaining coefficients will be missing as per
#'   `x_cols_aliased`. Otherwise the function fails for rank deficient design matrices.
#' @param drop_visit_levels (`flag`)\cr whether to drop levels for visit variable, if visit variable is a factor.
#'
#' @return List of class `mmrm_tmb_data` with elements:
#' - `full_frame`: `data.frame` with `n` rows containing all variables needed in the model.
#' - `x_matrix`: `matrix` with `n` rows and `p` columns specifying the overall design matrix.
#' - `x_cols_aliased`: `logical` with potentially more than `p` elements indicating which
#'      columns in the original design matrix have been left out to obtain a full rank
#'      `x_matrix`.
#' - `y_vector`: length `n` `numeric` specifying the overall response vector.
#' - `weights_vector`: length `n` `numeric` specifying the weights vector.
#' - `visits_zero_inds`: length `n` `integer` containing zero-based visits indices.
#' - `n_visits`: `int` with the number of visits, which is the dimension of the
#'      covariance matrix.
#' - `n_subjects`: `int` with the number of subjects.
#' - `subject_zero_inds`: length `n_subjects` `integer` containing the zero-based start
#'     indices for each subject.
#' - `subject_n_visits`: length `n_subjects` `integer` containing the number of
#'     observed visits for each subjects. So the sum of this vector equals `n`.
#' - `cov_type`: `string` value specifying the covariance type.
#' - `is_spatial_int`: `int` specifying whether the covariance structure is spatial(1) or not(0).
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
                            weights,
                            reml,
                            accept_singular,
                            drop_visit_levels) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_data_frame(data)
  varname <- formula_parts[grepl("_var", names(formula_parts))]
  assert_names(
    names(data),
    must.include = unlist(varname, use.names = FALSE)
  )
  assert_true(is.factor(data[[formula_parts$subject_var]]) || is.character(data[[formula_parts$subject_var]]))
  assert_numeric(weights, len = nrow(data))
  assert_flag(reml)
  assert_flag(accept_singular)
  assert_flag(drop_visit_levels)

  if (is.character(data[[formula_parts$subject_var]])) {
    data[[formula_parts$subject_var]] <- factor(
      data[[formula_parts$subject_var]],
      levels = stringr::str_sort(unique(data[[formula_parts$subject_var]]), numeric = TRUE)
    )
  }
  data_order <- if (formula_parts$is_spatial) {
    order(data[[formula_parts$subject_var]])
  } else {
    subject_visit_data <- data[, c(formula_parts$subject_var, formula_parts$visit_var)]
    is_duplicated <- duplicated(subject_visit_data)
    if (any(is_duplicated)) {
      stop(
        "time points have to be unique for each subject, detected following duplicates in data:\n",
        paste(utils::capture.output(print(subject_visit_data[is_duplicated, ])), collapse = "\n")
      )
    }
    order(data[[formula_parts$subject_var]], data[[formula_parts$visit_var]])
  }
  data <- data[data_order, ]
  weights <- weights[data_order]
  data <- data.frame(data, weights)
  # weights is always the last column
  weights_name <- colnames(data)[ncol(data)]
  full_frame <- eval(
    bquote(stats::model.frame(formula_parts$full_formula, data = data, weights = .(as.symbol(weights_name))))
  )
  full_frame <- droplevels(full_frame, except = formula_parts$visit_var)
  if (drop_visit_levels && !formula_parts$is_spatial && is.factor(full_frame[[formula_parts$visit_var]])) {
    old_levels <- levels(full_frame[[formula_parts$visit_var]])
    full_frame[[formula_parts$visit_var]] <- droplevels(full_frame[[formula_parts$visit_var]])
    new_levels <- levels(full_frame[[formula_parts$visit_var]])
    dropped <- setdiff(old_levels, new_levels)
    if (length(dropped) > 0) {
      message("In ", formula_parts$visit_var, " there are dropped visits: ", toString(dropped))
    }
  }

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
  weights_vector <- as.numeric(stats::model.weights(full_frame))
  n_subjects <- nlevels(full_frame[[formula_parts$subject_var]])
  subject_zero_inds <- which(!duplicated(full_frame[[formula_parts$subject_var]])) - 1L
  subject_n_visits <- c(utils::tail(subject_zero_inds, -1L), nrow(full_frame)) - subject_zero_inds
  assert_true(identical(subject_n_visits, as.integer(table(full_frame[[formula_parts$subject_var]]))))
  assert_true(all(subject_n_visits > 0))
  if (!is.null(formula_parts$group_var)) {
    assert_factor(data[[formula_parts$group_var]])
    subject_groups <- full_frame[[formula_parts$group_var]][subject_zero_inds + 1L]
    n_groups <- nlevels(subject_groups)
  } else {
    subject_groups <- factor(rep(0L, n_subjects))
    n_groups <- 1L
  }
  coordinates <- full_frame[, formula_parts$visit_var, drop = FALSE]
  if (formula_parts$is_spatial) {
    lapply(coordinates, assert_numeric)
    coordinates_matrix <- as.matrix(coordinates)
    visits_zero_inds <- 0L
    n_visits <- max(subject_n_visits)
  } else {
    assert(identical(ncol(coordinates), 1L))
    assert_factor(coordinates[[1L]])
    visits_zero_inds <- as.integer(coordinates[[1L]]) - 1L
    coordinates_matrix <- as.matrix(visits_zero_inds, ncol = 1)
    n_visits <- nlevels(coordinates[[1L]])
    assert_true(all(subject_n_visits <= n_visits))
  }
  structure(
    list(
      full_frame = full_frame,
      x_matrix = x_matrix,
      x_cols_aliased = x_cols_aliased,
      coordinates = coordinates_matrix,
      y_vector = y_vector,
      weights_vector = weights_vector,
      visits_zero_inds = visits_zero_inds,
      n_visits = n_visits,
      n_subjects = n_subjects,
      subject_zero_inds = subject_zero_inds,
      subject_n_visits = subject_n_visits,
      cov_type = formula_parts$cov_type,
      is_spatial_int = as.integer(formula_parts$is_spatial),
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
  start_value <- switch(formula_parts$cov_type,
    us = rep(0, m * (m + 1) / 2),
    toep = rep(0, m),
    toeph = rep(0, 2 * m - 1),
    ar1 = c(0, 0.5),
    ar1h = c(rep(0, m), 0.5),
    ad = rep(0, m),
    adh = rep(0, 2 * m - 1),
    cs = rep(0, 2),
    csh = rep(0, m + 1),
    sp_exp = rep(0, 2)
  )
  theta_dim <- length(start_value) * n_groups
  if (!is.null(start)) {
    assert_numeric(start, len = theta_dim, any.missing = FALSE, finite = TRUE)
  } else {
    start <- rep(start_value, n_groups)
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

#' Checking the `TMB` Optimization Result
#'
#' @param tmb_opt (`list`)\cr optimization result.
#' @param mmrm_tmb (`mmrm_tmb`)\cr result from [h_mmrm_tmb_fit()].
#'
#' @return Nothing, only used to generate warnings in case that the model
#' did not converge.
#'
#' @keywords internal
h_mmrm_tmb_check_conv <- function(tmb_opt,
                                  mmrm_tmb) {
  assert_list(tmb_opt)
  assert_subset(c("par", "objective", "convergence", "message"), names(tmb_opt))
  assert_class(mmrm_tmb, "mmrm_tmb")

  if (!is.null(tmb_opt$convergence) && tmb_opt$convergence != 0) {
    warning("Model convergence problem: ", tmb_opt$message, ".")
  } else {
    theta_vcov <- mmrm_tmb$theta_vcov
    if (is(theta_vcov, "try-error")) {
      warning("Model convergence problem: hessian is singular, theta_vcov not available")
    }
  }
}

#' Extract covariance matrix from `TMB` report and input data
#'
#' This helper does some simple post-processing to extract covariance matrix or named
#' list of covariance matrices if the fitting is using grouped covariance matrices.
#'
#' @param tmb_report (`list`)\cr report created with [TMB::MakeADFun()] report function.
#' @param tmb_data (`mmrm_tmb_data`)\cr produced by [h_mmrm_tmb_data()].
#' @param visit_var `character`\cr character vector of the visit variable
#' @param is_spatial `logical`\cr logical value indicating the covariance structure is spatial.
#' @return Return a simple covariance matrix if there is no grouping, or a named
#' list of estimated grouped covariance matrices,
#' with its name equal to the group levels.
#'
#' @keywords internal
h_mmrm_tmb_extract_cov <- function(tmb_report, tmb_data, visit_var, is_spatial) {
  d <- dim(tmb_report$covariance_lower_chol)
  visit_names <- if (!is_spatial) {
    levels(tmb_data$full_frame[[visit_var]])
  } else {
    c(0, 1)
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
#' @param weights (`vector`)\cr weights to be used in the fitting process.
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
#'   - `weights`: input.
#'   - `reml`: input as a flag.
#'   - `opt_details`: list with optimization details including convergence code.
#'   - `tmb_object`: original `TMB` object created with [TMB::MakeADFun()].
#'   - `tmb_data`: input.
#'
#' @keywords internal
h_mmrm_tmb_fit <- function(tmb_object,
                           tmb_opt,
                           data,
                           weights,
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
  cov <- h_mmrm_tmb_extract_cov(tmb_report, tmb_data, formula_parts$visit_var, formula_parts$is_spatial)
  beta_est <- tmb_report$beta
  names(beta_est) <- x_matrix_cols
  beta_vcov <- tmb_report$beta_vcov
  dimnames(beta_vcov) <- list(x_matrix_cols, x_matrix_cols)
  theta_est <- tmb_opt$par
  names(theta_est) <- NULL
  theta_vcov <- try(solve(tmb_object$he(tmb_opt$par)), silent = TRUE)
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
      weights = weights,
      reml = as.logical(tmb_data$reml),
      opt_details = tmb_opt[opt_details_names],
      tmb_object = tmb_object,
      tmb_data = tmb_data
    ),
    class = "mmrm_tmb"
  )
}

#' Low-Level Fitting Function for MMRM
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
#' @param weights (`vector`)\cr input vector containing the weights.
#' @inheritParams h_mmrm_tmb_data
#' @param control (`mmrm_control`)\cr list of control options produced
#'   by [mmrm_control()].
#'
#' @return List of class `mmrm_tmb`, see [h_mmrm_tmb_fit()] for details.
#'
#' @details The `formula` typically looks like:
#' `FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)`
#' so specifies response and covariates as usual, and exactly one special term
#' defines which covariance structure is used and what are the visit and
#' subject variables.
#' Always use only the first optimizer if multiple optimizers are provided.
#'
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' data <- fev_data
#' system.time(result <- fit_mmrm(formula, data, rep(1, nrow(fev_data))))
fit_mmrm <- function(formula,
                     data,
                     weights,
                     reml = TRUE,
                     control = mmrm_control()) {
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  assert_class(control, "mmrm_control")
  assert_list(control$optimizers, min.len = 1)
  assert_numeric(weights, any.missing = FALSE)
  assert_true(all(weights > 0))
  tmb_data <- h_mmrm_tmb_data(
    formula_parts, data, weights, reml,
    accept_singular = control$accept_singular, drop_visit_levels = control$drop_visit_levels
  )
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start = control$start, n_groups = tmb_data$n_groups)

  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  h_mmrm_tmb_assert_start(tmb_object)
  used_optimizer <- control$optimizers[[1]]
  args <- with(
    tmb_object,
    c(
      list(par, fn, gr),
      attr(used_optimizer, "args")
    )
  )
  if (identical(attr(used_optimizer, "use_hessian"), TRUE)) {
    args$hessian <- tmb_object$he
  }
  tmb_opt <- do.call(
    what = used_optimizer,
    args = args
  )
  # Ensure negative log likelihood is stored in `objective` element of list.
  if ("value" %in% names(tmb_opt)) {
    tmb_opt$objective <- tmb_opt$value
    tmb_opt$value <- NULL
  }
  fit <- h_mmrm_tmb_fit(tmb_object, tmb_opt, data, weights, formula_parts, tmb_data)
  h_mmrm_tmb_check_conv(tmb_opt, fit)

  fun_call <- match.call()
  fun_call$formula <- eval(formula_parts$formula)
  fun_call$data <- ifelse(
    !is.null(attr(data, which = "dataname")),
    attr(data, which = "dataname"),
    toString(match.call()$data)
  )
  weights_str <- attr(weights, which = "dataname")
  if (!is.null(weights_str)) {
    fun_call$weights <- weights_str
  }
  fit$call <- fun_call
  fit
}
