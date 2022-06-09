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
#' - `model_formula`: `formula` with the correlation term is removed.
#' - `full_formula`: same as `model_formula` but includes the `subject_var`.
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
  model_formula <- stats::update(formula, as.formula(paste(". ~ . -", deparse(corr_term))))

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
  full_formula <- stats::update(model_formula, as.formula(paste(". ~ . +", subject_var)))

  structure(
    list(
      model_formula = model_formula,
      full_formula = full_formula,
      corr_type = deparse(corr_term[[1L]]),
      visit_var = deparse(visit_term),
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
#'
#' @export
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' formula_parts <- h_mmrm_tmb_formula_parts(formula)
#' tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data)
h_mmrm_tmb_data <- function(formula_parts,
                            data) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_data_frame(data)
  assert_names(
    names(data),
    must.include = c(formula_parts$visit_var, formula_parts$subject_var)
  )
  assert_factor(data[[formula_parts$visit_var]])
  assert_factor(data[[formula_parts$subject_var]])

  data <- data[order(data[[formula_parts$subject_var]], data[[formula_parts$visit_var]]), ]
  full_frame <- droplevels(stats::model.frame(formula_parts$full_formula, data = data))
  x_matrix <- stats::model.matrix(formula_parts$model_formula, data = full_frame)
  y_vector <- stats::model.response(full_frame)
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
      x_matrix = x_matrix,
      y_vector = y_vector,
      visits_zero_inds = visits_zero_inds,
      n_visits = n_visits,
      n_subjects = n_subjects,
      subject_inds = subject_zero_inds,
      subject_n_visits = subject_n_visits,
      corr_type = corr_type
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
#' tmb_data <- h_mmrm_tmb_data(formula_parts, fev_data)
#' pars <- h_mmrm_tmb_parameters(formula_parts, tmb_data, NULL)
h_mmrm_tmb_parameters <- function(formula_parts,
                                  tmb_data,
                                  start_values) {
  assert_class(formula_parts, "mmrm_tmb_formula_parts")
  assert_class(tmb_data, "mmrm_tmb_data")

  n <- tmb_data$n_visits
  theta_dim <- as.integer(switch(formula_parts$corr_type,
    us = n * (n + 1) / 2,
    toep = 0,
    ar1 = 0,
    cs = 0,
    ad = 0
  ))
  if (!is.null(start_values)) {
    assert_numeric(start_values, len = theta_dim, any.missing = FALSE, finite = TRUE)
  } else {
    start_values <- rep(0, theta_dim)
  }
  list(theta = start_values)
}

#' Fitting an MMRM with `TMB`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula (`formula`)\cr model formula with exactly one special term
#'   specifying the visits within subjects, see details.
#' @param data (`data.frame`)\cr input data containing the variables used
#'   in `formula`.
#' @param start (`numeric` or `NULL`)\cr optional start values for variance
#'   parameters.
#' @param control (`mmrm_tmb_control`)\cr list of control options produced
#'   by [h_mmrm_tmb_control()].
#'
#' @return List of class `mmrm_tmb` with:
#' - `object`: The `TMB` object.
#' - `fit`: The fit result.
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
#' result <- mmrm_tmb(formula, data)
mmrm_tmb <- function(formula,
                     data,
                     start = NULL,
                     control = h_mmrm_tmb_control()) {
  formula_parts <- h_mmrm_tmb_formula_parts(formula)
  tmb_data <- h_mmrm_tmb_data(formula_parts, data)
  tmb_parameters <- h_mmrm_tmb_parameters(formula_parts, tmb_data, start)
  assert_class(control, "mmrm_tmb_control")

  tmb_object <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_parameters,
    hessian = TRUE,
    DLL = "mmrm",
    silent = TRUE
  )
  tmb_fit <- with(
    tmb_object,
    do.call(
      what = control$optimizer,
      args = c(
        list(par, fn, gr, control = control$optimizer_control),
        control$optimizer_args
      )
    )
  )
  structure(
    list(
      object = tmb_object,
      fit = tmb_fit
    ),
    class = "mmrm_tmb"
  )
}
