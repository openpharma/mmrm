#' Capture all Output
#'
#' This function silences all warnings, errors & messages and instead returns a list
#' containing the results (if it didn't error), as well as the warnings, errors
#' and messages and divergence signals as character vectors.
#'
#' @param expr (`expression`)\cr to be executed.
#' @param remove (`list`)\cr optional list with elements `warnings`, `errors`,
#'   `messages` which can be character vectors, which will be removed from the
#'   results if specified.
#' @param divergence (`list`)\cr optional list similar as `remove`, but these
#'   character vectors will be moved to the `divergence` result and signal
#'   that the fit did not converge.
#'
#' @return
#' A list containing
#'
#' - `result`: The object returned by `expr` or `list()` if an error was thrown.
#' - `warnings`: `NULL` or a character vector if warnings were thrown.
#' - `errors`: `NULL` or a string if an error was thrown.
#' - `messages`: `NULL` or a character vector if messages were produced.
#' - `divergence`: `NULL` or a character vector if divergence messages were caught.
#'
#' @keywords internal
h_record_all_output <- function(expr,
                                remove = list(),
                                divergence = list()) {
  # Note: We don't need to and cannot assert `expr` here.
  assert_list(remove, types = "character")
  assert_list(divergence, types = "character")
  env <- new.env()
  result <- withCallingHandlers(
    withRestarts(
      expr,
      muffleStop = function(e) structure(e$message, class = "try-error")
    ),
    message = function(m) {
      msg_without_newline <- gsub(m$message, pattern = "\n$", replacement = "")
      env$message <- c(env$message, msg_without_newline)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      env$warning <- c(env$warning, w$message)
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      env$error <- c(env$error, e$message)
      invokeRestart("muffleStop", e)
    }
  )
  list(
    result = result,
    warnings = setdiff(env$warning, c(remove$warnings, divergence$warnings)),
    errors = setdiff(env$error, c(remove$errors, divergence$errors)),
    messages = setdiff(env$message, c(remove$messages, divergence$messages)),
    divergence = c(
      intersect(env$warning, divergence$warnings),
      intersect(env$error, divergence$errors),
      intersect(env$message, divergence$messages)
    )
  )
}

#' Trace of a Matrix
#'
#' @description Obtain the trace of a matrix if the matrix is diagonal, otherwise raise an error.
#'
#' @param x (`matrix`)\cr square matrix input.
#'
#' @return The trace of the square matrix.
#'
#' @keywords internal
h_tr <- function(x) {
  if (nrow(x) != ncol(x)) {
    stop("x must be square matrix")
  }
  sum(Matrix::diag(x))
}

#' Split Control List
#'
#' @description Split the [mmrm_control()] object according to its optimizers and use additional arguments
#' to replace the elements in the original object.
#'
#' @param control (`mmrm_control`)\cr object.
#' @param ... additional parameters to update the `control` object.
#'
#' @return A `list` of `mmrm_control` entries.
#' @keywords internal
h_split_control <- function(control, ...) {
  assert_class(control, "mmrm_control")
  l <- length(control$optimizers)
  lapply(seq_len(l), function(i) {
    ret <- utils::modifyList(control, list(...))
    ret$optimizers <- control$optimizers[i]
    ret
  })
}

#' Obtain Optimizer according to Optimizer String Value
#'
#' @description This function creates optimizer functions with arguments.
#'
#' @param optimizer (`character`)\cr names of built-in optimizers to try, subset
#'   of "L-BFGS-B", "BFGS", "CG" and "nlminb".
#' @param optimizer_fun (`function` or `list` of `function`)\cr alternatively to `optimizer`,
#'   an optimizer function or a list of optimizer functions can be passed directly here.
#' @param optimizer_args (`list`)\cr additional arguments for `optimizer_fun`.
#' @param optimizer_control (`list`)\cr passed to argument `control` in `optimizer_fun`.
#'
#' @details
#' If you want to use only the built-in optimizers:
#' - `optimizer` is a shortcut to create a list of built-in optimizer functions
#'   passed to `optimizer_fun`.
#' - Allowed are "L-BFGS-B", "BFGS", "CG" (using [stats::optim()] with corresponding method)
#'   and "nlminb" (using [stats::nlminb()]).
#' - Other arguments should go into `optimizer_args`.
#'
#' If you want to use your own optimizer function:
#' - Make sure that there are three arguments: parameter (start value), objective function
#'   and gradient function are sequentially in the function arguments.
#' - If there are other named arguments in front of these, make sure they are correctly
#'   specified through `optimizer_args`.
#' - If the hessian can be used, please make sure its argument name is `hessian` and
#'   please add attribute `use_hessian = TRUE` to the function,
#'   using `attr(fun, "use_hessian) <- TRUE`.
#'
#' @return Named `list` of optimizers created by [h_partial_fun_args()].
#'
#' @keywords internal
h_get_optimizers <- function(optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb"),
                             optimizer_fun = h_optimizer_fun(optimizer),
                             optimizer_args = list(),
                             optimizer_control = list()) {
  if ("automatic" %in% optimizer) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = I("\"automatic\" optimizer"),
      details = "please just omit optimizer argument"
    )
    optimizer_fun <- h_optimizer_fun()
  }
  assert(
    test_function(optimizer_fun),
    test_list(optimizer_fun, types = "function", names = "unique")
  )
  if (is.function(optimizer_fun)) {
    optimizer_fun <- list(custom_optimizer = optimizer_fun)
  }
  lapply(optimizer_fun, function(x) {
    do.call(h_partial_fun_args, c(list(fun = x, control = optimizer_control), optimizer_args))
  })
}

#' Obtain Optimizer Function with Character
#' @description Obtain the optimizer function through the character provided.
#' @param optimizer (`character`)\cr vector of optimizers.
#'
#' @return A (`list`)\cr of optimizer functions generated from [h_partial_fun_args()].
#' @keywords internal
h_optimizer_fun <- function(optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb")) {
  optimizer <- match.arg(optimizer, several.ok = TRUE)
  lapply(stats::setNames(optimizer, optimizer), function(x) {
    switch(x,
      "L-BFGS-B" = h_partial_fun_args(fun = stats::optim, method = x),
      "BFGS" = h_partial_fun_args(fun = stats::optim, method = x),
      "CG" = h_partial_fun_args(fun = stats::optim, method = x),
      "nlminb" = h_partial_fun_args(fun = stats::nlminb, additional_attr = list(use_hessian = TRUE))
    )
  })
}

#' Create Partial Functions
#' @description Creates partial functions with arguments.
#'
#' @param fun (`function`)\cr to be wrapped.
#' @param ... Additional arguments for `fun`.
#' @param additional_attr (`list`)\cr of additional attributes to apply to the result.
#'
#' @details This function add `args` attribute to the original function,
#' and add an extra class `partial` to the function.
#' `args` is the argument for the function, and elements in `...` will override the existing
#' arguments in attribute `args`. `additional_attr` will override the existing attributes.
#'
#' @return Object with S3 class `"partial"`, a `function` with `args` attribute (and possibly more
#' attributes from `additional_attr`).
#' @keywords internal
h_partial_fun_args <- function(fun, ..., additional_attr = list()) {
  assert_function(fun)
  assert_list(additional_attr, names = "unique")
  a_args <- list(...)
  assert_list(a_args, names = "unique")
  args <- attr(fun, "args")
  if (is.null(args)) {
    args <- list()
  }
  do.call(
    structure,
    args = utils::modifyList(
      list(
        .Data = fun,
        args = utils::modifyList(args, a_args),
        class = c("partial", "function")
      ),
      additional_attr
    )
  )
}

#' Obtain Default Covariance Method
#'
#' @description Obtain the default covariance method depending on
#' the degrees of freedom method used.
#'
#' @param method (`string`)\cr degrees of freedom method.
#'
#' @details The default covariance method is different for different degrees of freedom method.
#' For "Satterthwaite" or "Between-Within", "Asymptotic" is returned.
#' For "Kenward-Roger" only, "Kenward-Roger" is returned.
#' For "Residual" only, "Empirical" is returned.
#'
#' @return String of the default covariance method.
#' @keywords internal
h_get_cov_default <- function(method = c("Satterthwaite", "Kenward-Roger", "Residual", "Between-Within")) {
  assert_string(method)
  method <- match.arg(method)
  switch(method,
    "Residual" = "Empirical",
    "Satterthwaite" = "Asymptotic",
    "Kenward-Roger" = "Kenward-Roger",
    "Between-Within" = "Asymptotic"
  )
}

#' Complete `character` Vector Names From Values
#'
#' @param x (`character` or `list`)\cr value whose names should be completed
#'   from element values.
#'
#' @return A named vector or list.
#'
#' @keywords internal
fill_names <- function(x) {
  n <- names(x)
  is_unnamed <- if (is.null(n)) rep_len(TRUE, length(x)) else n == ""
  names(x)[is_unnamed] <- x[is_unnamed]
  x
}

#' Drop Items from an Indexible
#'
#' Drop elements from an indexible object (`vector`, `list`, etc.).
#'
#' @param x Any object that can be consumed by [seq_along()] and indexed by a
#'   logical vector of the same length.
#' @param n (`integer`)\cr the number of terms to drop.
#'
#' @return A subset of `x`.
#'
#' @keywords internal
drop_elements <- function(x, n) {
  x[seq_along(x) > n]
}

#' Ask for Confirmation on Large Visit Levels
#'
#' @description Ask the user for confirmation if there are too many visit levels
#' for non-spatial covariance structure in interactive sessions.
#'
#' @param x (`numeric`)\cr number of visit levels.
#'
#' @return Logical value `TRUE`.
#' @keywords internal
h_confirm_large_levels <- function(x) {
  assert_count(x)
  allowed_lvls <- x <= getOption("mmrm.max_visits", 100)
  if (allowed_lvls) {
    return(TRUE)
  }
  if (!interactive()) {
    stop("Visit levels too large!", call. = FALSE)
  }
  proceed <- utils::askYesNo(
    paste(
      "Visit levels is possibly too large.",
      "This requires large memory. Are you sure to continue?",
      collapse = " "
    )
  )
  if (!identical(proceed, TRUE)) {
    stop("Visit levels too large!", call. = FALSE)
  }
  TRUE
}

#' Default Value on NULL
#' Return default value when first argument is NULL.
#'
#' @param x Object.
#' @param y Object.
#'
#' @details If `x` is NULL, returns `y`. Otherwise return `x`.
#'
#' @keywords internal
h_default_value <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' Warn on na.action
#' @keywords internal
h_warn_na_action <- function() {
  if (!identical(getOption("na.action"), "na.omit")) {
    warning("na.action is always set to `na.omit` for `mmrm` fit!")
  }
}

#' Obtain `na.action` as Function
#' @keywords internal
h_get_na_action <- function(na_action) {
  if (is.function(na_action) && identical(methods::formalArgs(na_action), c("object", "..."))) {
    return(na_action)
  }
  if (is.character(na_action) && length(na_action) == 1L) {
    assert_subset(na_action, c("na.omit", "na.exclude", "na.fail", "na.pass", "na.contiguous"))
    get(na_action, mode = "function", pos = "package:stats")
  }
}

#' Validate mmrm Formula
#' @param formula (`formula`)\cr to check.
#'
#' @details In mmrm models, `.` is not allowed as it introduces ambiguity of covariates
#' to be used, so it is not allowed to be in formula.
#'
#' @keywords internal
h_valid_formula <- function(formula) {
  assert_formula(formula)
  if ("." %in% all.vars(formula)) {
    stop("`.` is not allowed in mmrm models!")
  }
}

#' Standard Starting Value
#'
#' @description Obtain standard start values.
#'
#' @param cov_type (`string`)\cr name of the covariance structure.
#' @param n_visits (`int`)\cr number of visits.
#' @param n_groups (`int`)\cr number of groups.
#' @param ... not used.
#'
#' @details
#' `std_start` will try to provide variance parameter from identity matrix.
#' However, for `ar1` and `ar1h` the corresponding values are not ideal because the
#' \eqn{\rho} is usually a positive number thus using 0 as starting value can lead to
#' incorrect optimization result, and we use 0.5 as the initial value of \eqn{\rho}.
#'
#' @return A numeric vector of starting values.
#'
#' @export
std_start <- function(cov_type, n_visits, n_groups, ...) {
  assert_string(cov_type)
  assert_subset(cov_type, cov_types(c("abbr", "habbr")))
  assert_int(n_visits, lower = 1L)
  assert_int(n_groups, lower = 1L)
  start_value <- switch(cov_type,
    us = rep(0, n_visits * (n_visits + 1) / 2),
    toep = rep(0, n_visits),
    toeph = rep(0, 2 * n_visits - 1),
    ar1 = c(0, 0.5),
    ar1h = c(rep(0, n_visits), 0.5),
    ad = rep(0, n_visits),
    adh = rep(0, 2 * n_visits - 1),
    cs = rep(0, 2),
    csh = rep(0, n_visits + 1),
    sp_exp = rep(0, 2)
  )
  rep(start_value, n_groups)
}

#' Empirical Starting Value
#'
#' @description Obtain empirical start value for unstructured covariance
#'
#' @param data (`data.frame`)\cr data used for model fitting.
#' @param model_formula (`formula`)\cr the formula in mmrm model without covariance structure part.
#' @param visit_var (`string`)\cr visit variable.
#' @param subject_var (`string`)\cr subject id variable.
#' @param subject_groups (`factor`)\cr subject group assignment.
#' @param ... not used.
#'
#' @details
#' This `emp_start` only works for unstructured covariance structure.
#' It uses linear regression to first obtain the coefficients and use the residuals
#' to obtain the empirical variance-covariance, and it is then used to obtain the
#' starting values.
#'
#' @note `data` is used instead of `full_frame` because `full_frame` is already
#' transformed if model contains transformations, e.g. `log(FEV1) ~ exp(FEV1_BL)` will
#' drop `FEV1` and `FEV1_BL` but add `log(FEV1)` and `exp(FEV1_BL)` in `full_frame`.
#'
#' @return A numeric vector of starting values.
#'
#' @export
emp_start <- function(data, model_formula, visit_var, subject_var, subject_groups, ...) {
  assert_formula(model_formula)
  assert_data_frame(data)
  assert_subset(all.vars(model_formula), colnames(data))
  assert_string(visit_var)
  assert_string(subject_var)
  assert_factor(data[[visit_var]])
  n_visits <- length(levels(data[[visit_var]]))
  assert_factor(data[[subject_var]])
  subjects <- droplevels(data[[subject_var]])
  n_subjects <- length(levels(subjects))
  fit <- stats::lm(formula = model_formula, data = data)
  res <- rep(NA, n_subjects * n_visits)
  res[
    n_visits * as.integer(subjects) - n_visits + as.integer(data[[visit_var]])
  ] <- residuals(fit)
  res_mat <- matrix(res, ncol = n_visits, nrow = n_subjects, byrow = TRUE)
  emp_covs <- lapply(
    unname(split(seq_len(n_subjects), subject_groups)),
    function(x) {
      stats::cov(res_mat[x, , drop = FALSE], use = "pairwise.complete.obs")
    }
  )
  unlist(lapply(emp_covs, h_get_theta_from_cov))
}
#' Obtain Theta from Covariance Matrix
#'
#' @description Obtain unstructured theta from covariance matrix.
#'
#' @param covariance (`matrix`) of covariance matrix values.
#'
#' @details
#' If the covariance matrix has `NA` in some of the elements, they will be replaced by
#' 0 (non-diagonal) and 1 (diagonal). This ensures that the matrix is positive definite.
#'
#' @return Numeric vector of the theta values.
#' @keywords internal
h_get_theta_from_cov <- function(covariance) {
  assert_matrix(covariance, mode = "numeric", ncols = nrow(covariance))
  covariance[is.na(covariance)] <- 0
  diag(covariance)[diag(covariance) == 0] <- 1
  # empirical is not always positive definite in some special cases of numeric singularity.
  qr_res <- qr(covariance)
  if (qr_res$rank < ncol(covariance)) {
    covariance <- Matrix::nearPD(covariance)$mat
  }
  emp_chol <- t(chol(covariance))
  mat <- t(solve(diag(diag(emp_chol)), emp_chol))
  ret <- c(log(diag(emp_chol)), mat[upper.tri(mat)])
  unname(ret)
}

#' Register S3 Method
#' Register S3 method to a generic.
#'
#' @param pkg (`string`) name of the package name.
#' @param generic (`string`) name of the generic.
#' @param class (`string`) class name the function want to dispatch.
#' @param envir (`environment`) the location the method is defined.
#'
#' @details This function is adapted from `emmeans:::register_s3_method()`.
#'
#' @keywords internal
h_register_s3 <- function(pkg, generic, class, envir = parent.frame()) {
  assert_string(pkg)
  assert_string(generic)
  assert_string(class)
  assert_environment(envir)
  fun <- get(paste0(generic, ".", class), envir = envir)
  if (isNamespaceLoaded(pkg)) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}

#' Check if a Factor Should Drop Levels
#'
#' @param x (`vector`) vector to check.
#'
#' @keywords internal
h_extra_levels <- function(x) {
  is.factor(x) && length(levels(x)) > length(unique(x))
}

#' Drop Levels from Dataset
#' @param data (`data.frame`) data to drop levels.
#' @param subject_var (`character`) subject variable.
#' @param visit_var (`character`) visit variable.
#' @param except (`character`) variables to exclude from dropping.
#' @keywords internal
h_drop_levels <- function(data, subject_var, visit_var, except) {
  assert_data_frame(data)
  assert_character(subject_var)
  assert_character(visit_var)
  assert_character(except, null.ok = TRUE)
  all_cols <- colnames(data)
  to_drop <- vapply(
    data,
    h_extra_levels,
    logical(1L)
  )
  to_drop <- all_cols[to_drop]
  # only drop levels for those not defined in excep and not in visit_var.
  to_drop <- setdiff(to_drop, c(visit_var, except))
  data[to_drop] <- lapply(data[to_drop], droplevels)
  # subject var are always dropped and no message given.
  dropped <- setdiff(to_drop, subject_var)
  if (length(dropped) > 0) {
    message(
      "Some factor levels are dropped due to singular design matrix: ",
      toString(dropped)
    )
  }
  data
}

#' Predicate if the TMB Version Used to Compile the Package is Sufficient
#'
#' @return Flag whether the TMB version is sufficient.
#' @keywords internal
h_tmb_version_sufficient <- function() {
  # Note: There is no version information saved in the dynamic library, but
  # we can check like this:
  tmb_config <- TMB::config(DLL = "mmrm")
  tape_deterministic <- tmb_config$tmbad_deterministic_hash
  !is.null(tape_deterministic)
}

#' Warn if TMB is Configured to Use Non-Deterministic Hash for Tape Optimizer
#'
#' This function checks the TMB configuration for the `tmbad_deterministic_hash` setting
#' If it is set to `FALSE`, a warning is issued indicating that this may lead to
#' unreproducible results.
#'
#' @return No return value, called for side effects.
#' @keywords internal
h_tmb_warn_non_deterministic <- function() {
  if (!h_tmb_version_sufficient()) {
    return()
  }
  tmb_config <- TMB::config(DLL = "mmrm")
  tape_deterministic <- tmb_config$tmbad_deterministic_hash
  if (!tape_deterministic) {
    msg <- paste(
      "TMB is configured to use a non-deterministic hash for its tape optimizer,",
      "and this may lead to unreproducible results.",
      "To disable this behavior, use `TMB::config(tmbad_deterministic_hash = 1)`.",
      sep = "\n"
    )
    warning(msg)
  }
}



#' Sort a Data Frame by All Its Columns in Ascending Order
#'
#' @param data (`data.frame`)\cr a data frame to be sorted.
#'
#' @returns A data frame. The same as the input `data` with columns in the same
#'   order but with rows sorted by the first column, with ties broken by the
#'   second column, and so forth.
#' @keywords internal
h_dataset_sort_all <- function(data) {
  assert_data_frame(data)
  data[do.call(order, unname(data)), , drop = FALSE]
}



#' Predicate Indicating Whether Two Datasets Contain the Same Observations
#'
#' Checks whether two datasets contain the same observations and that the first
#' dataset's columns are a subset of the second dataset's columns.
#'
#' For efficiency, the inspection takes place in this order:
#'
#' 1. `FALSE` is returned early if the datasets do not have the same number of
#' rows.
#'
#' 1. `FALSE` is returned early if the first dataset has a column not in the
#' second dataset.
#'
#' 1. The columns in common are sorted and compared using [all.equal()] with
#' `check.attributes = FALSE`.
#'
#' @param data_basic,data_augmented (`data.frame`)\cr data frames to be
#'   compared.
#'
#' @returns `TRUE` or `FALSE`, indicating whether the or not `data_basic` and
#'   `data_augmented` contain the same observations and that `data_augmented`
#'   contains all the columns in `data_basic`.
#' @seealso [h_check_fits_all_data_same()], which performs this check on the
#'   datasets of adjacent elements of a list of `mmrm` fits.
#' @keywords internal
h_check_columns_nested <- function(data_basic, data_augmented) {
  assert_data_frame(data_basic)
  assert_data_frame(data_augmented)
  if (nrow(data_basic) != nrow(data_augmented)) {
    return(FALSE)
  }

  colnames_basic <- colnames(data_basic)

  if (anyNA(match(colnames_basic, colnames(data_augmented)))) {
    return(FALSE)
  }

  isTRUE(all.equal(
    h_dataset_sort_all(data_basic),
    h_dataset_sort_all(data_augmented[colnames_basic]),
    check.attributes = FALSE
  ))
}



#' Predicate Indicating Whether `mmrm` Fits' Datasets Contain the Same
#' Observations
#'
#' Checks a `list` of `mmrm` fits to see whether all their datasets contain the
#' same observations and that they only increase in columns from one dataset to
#' the next (i.e, columns are nested).
#'
#' For efficiency, the inspection takes place in this order:
#'
#' 1. `FALSE` is returned early if not all datasets have the same number of
#' rows.
#'
#' 1. `FALSE` is returned early if a dataset has a column not in the next
#' dataset.
#'
#' 1. The columns in common among adjacent datasets are sorted and compared
#' using [all.equal()] with `check.attributes = FALSE`.
#'
#' This function is more efficient than running [h_check_columns_nested()] on
#' all adjacent pairs and supplying the results to [all()].
#'
#' @param fits (`list`)\cr list of `mmrm` fits.
#'
#' @returns `TRUE` or `FALSE` indicating whether or not the datasets underlying
#'   the elements of `fits` contain the same observations, and that each
#'   dataset's columns are a subset of the next dataset's columns.
#' @seealso [h_check_columns_nested()], which performs this check on two data
#'   sets.
#' @keywords internal
h_check_fits_all_data_same <- function(fits) {
  assert_list(fits, types = "mmrm", any.missing = FALSE, min.len = 1)
  datasets <- lapply(fits, h_get_minimal_fit_data)

  # Is nrow() the same for all datasets?
  datasets_nrows <- vapply(datasets, nrow, numeric(1L))
  if (length(unique(datasets_nrows)) > 1L) {
    return(FALSE)
  }

  cols <- lapply(datasets, colnames)

  for (i in seq_along(fits)[-1L]) {
    # Ensure previous dataset's cols are a subset of the current dataset's cols
    if (anyNA(match(cols[[i - 1L]], cols[[i]]))) {
      return(FALSE)
    }
  }

  datasets[[1L]] <- h_dataset_sort_all(datasets[[1L]])
  for (i in seq_along(fits)[-1L]) {

    # Pull prev dataset's columns to the front of current dataset. Then sort.
    cols[[i]] <- union(cols[[i - 1L]], cols[[i]])
    datasets[[i]] <- h_dataset_sort_all(datasets[[i]][cols[[i]]])

    # Ensure the common columns are the same
    if (
      !isTRUE(all.equal(
        datasets[[i - 1L]],
        datasets[[i]][cols[[i - 1L]]],
        check.attributes = FALSE
      ))
    ) {
      return(FALSE)
    }
  }

  TRUE
}



#' Combine the Datasets from `mmrm` Fits
#'
#' Take the data columns used in each `mmrm` fit and [merge()] them.
#'
#' All default arguments for [merge()] are used, resulting in a "natural join":
#' the result will only contain the observations found in all datasets.
#'
#' [droplevels()] is applied to the final product to prevent extraneous
#' warnings.
#'
#' @param fits (`list`)\cr list of `mmrm` fits.
#'
#' @returns A data frame combining all the common observations among the
#'   datasets underlying the elements of `fits`.
#' @keywords internal
h_fits_common_data <- function(fits) {
  assert_list(fits, types = "mmrm", any.missing = FALSE, min.len = 1)
  datasets <- lapply(fits, h_get_minimal_fit_data)
  out <- Reduce(merge, datasets)
  out <- droplevels(out)
  out
}



#' Obtain the Minimal Dataset Needed for an `mmrm` Fit
#'
#' Grab the dataset underlying an `mmrm` fit and select only the used columns.
#'
#' Grabs the response variable along with the predictors named in
#' `fit$formula_parts`.
#'
#' @param fit (`mmrm`)\cr a fitted `mmrm` model.
#'
#' @returns A data frame: a subset of the columns the dataset underlying `fit`
#'   (i.e., `fit$data`):
#'
#' - The response column.
#'
#' - The column denoting the visit index.
#'
#' - The column denoting the subject.
#'
#' - The column denoting the subject's grouping (e.g., study arm).
#'
#' - All other predictors not already specified.
#'
#'   Columns that were not used are excluded.
#'
#' @keywords internal
h_get_minimal_fit_data <- function(fit) {
  assert_class(fit, "mmrm")
  predictors <-
    fit[["formula_parts"]][
      c("visit_var", "subject_var", "group_var", "model_var")
    ]
  predictors <- unique(unlist(predictors, use.names = FALSE))
  terms_attr <- attributes(terms(fit))
  response <- as.character(terms_attr$variables[[terms_attr$response + 1]])
  fit[["data"]][c(response, predictors)]
}




#' Ensure LRT Is Appropriate for `list` of `mmrm` Fits
#'
#' Throws an error if the degrees of freedom aren't monotonically increasing, if
#' the models aren't nested, or if `refit = FALSE` and the models have different
#' underlying data.
#'
#' @param fits (`list`)\cr list of `mmrm` fits.
#' @param refit (`flag`)\cr `TRUE` or `FALSE` indicating whether or not the user
#'   gave permission to refit models to make all models suitable for LRT.
#' @param dfs (`numeric`)\cr vector of the degrees of freedom for each element
#'   of `fits`.
#' @param is_reml (`logical`)\cr vector indicating whether or not REML was used
#'   for each element of `fits`.
#'
#' @returns `TRUE` if the list of `fits` are suitable for LRT. Otherwise, an
#'   error is thrown.
#'
#' @keywords internal
h_assert_lrt_suitability <- function(fits, refit, dfs, is_reml) {

  assert_list(fits, types = "mmrm", min.len = 2)
  assert_flag(refit)
  assert_numeric(
    dfs,
    lower = 1,
    any.missing = FALSE,
    finite = TRUE,
    len = length(fits)
  )
  assert_logical(is_reml, any.missing = FALSE, len = length(fits))

  if (any(diff(dfs) <= 0)) {
    stop(
      "The degrees of freedom (df) of each model must be less than the df of ",
      "the next model in order to perform likelihood ratio testing (LRT). ",
      "Bypass LRT with test = FALSE.",
      call. = FALSE
    )
  }

  # Do any models use REML?
  any_reml <- any(is_reml)

  # First iterate through and check the models to make sure their covariates
  # and covariance structures are nested
  for (i in seq_along(fits)[-1L]) {
    h_assert_nested_models(fits[[i - 1L]], fits[[i]], any_reml = any_reml)
  }

  # If we didn't refit, throw errors if not all data are the same
  if (!refit && !h_check_fits_all_data_same(fits)) {
    stop(
      "Likelihood ratio testing requires all fits to use the same data.",
      " Not all fits have the same data and refit = FALSE.",
      " Consider setting test = FALSE or refit = TRUE.",
      call. = FALSE
    )
  }

  TRUE
}



#' Ensure Two Models Are Nested
#'
#' Throws an error if `model_basic` isn't nested within `model_augmented`.
#'
#' The following checks are applied in this order, and an error is thrown if any
#' of the conditions are not met:
#'
#' 1. The fits must have the same visit, subject, and grouping variables.
#'
#' 1. The covariates of `model_basic` must be the same as or a subset of the
#' covariates of `model_augmented`.
#'
#' 1. The covariance structure of `model_basic` must be the same as or a special
#' case of the covariance structure of `model_augmented`.
#'
#' Finally, if all these checks were passed, a warning is thrown if the two fits
#' have identical covariates and covariance structures.
#'
#' @param model_basic,model_augmented (`mmrm`)\cr model fits.
#' @param any_reml (`flag`)\cr `TRUE` or `FALSE` indicating whether or not
#'   either model used REML estimation.
#'
#' @returns `TRUE` if `model_basic` is nested within `model_augmented`.
#'   Otherwise, an error is thrown.
#'
#' @keywords internal
h_assert_nested_models <- function(model_basic, model_augmented, any_reml) {

  assert_class(model_basic, "mmrm")
  assert_class(model_augmented, "mmrm")
  assert_flag(any_reml)

  model_basic <- model_basic[["formula_parts"]]
  model_augmented <- model_augmented[["formula_parts"]]

  if (!identical(model_basic[["visit_var"]], model_augmented[["visit_var"]])) {
    stop("Models must all have the same visit variable.")
  }

  if (!identical(model_basic[["subject_var"]], model_augmented[["subject_var"]])) {
    stop("All models must have the same subject variable.")
  }

  if (!identical(model_basic[["group_var"]], model_augmented[["group_var"]])) {
    stop("All models must have the same grouping variable.")
  }

  covar_nesting <- h_check_covar_nesting(model_basic, model_augmented)
  if (any_reml && covar_nesting == "nested") {
    stop("If any models use REML, all models' covariates must be the same.")
  }

  cov_struct_nesting <- h_check_cov_struct_nesting(model_basic, model_augmented)

  if (covar_nesting == "identical" && cov_struct_nesting == "identical") {
    warning("Two models in the sequence have identical covariates and ",
            "covariance structures.")
  }

  TRUE
}


#' Ensure Two Models' Covariates Are Nested
#'
#' Throws an error if the covariates of `model_basic` aren't the same as or a
#' subset of the covariates of `model_augmented`.
#'
#' For upstream coding brevity, this function accepts the `formula_parts`
#' element of two `mmrm` model fits rather than `mmrm` objects. Such objects are
#' of class `mmrm_tmb_formula_parts`.
#'
#' @param model_basic,model_augmented (`mmrm_tmb_formula_parts`)\cr the
#'   `formula_parts` element of an `mmrm` model fit.
#'
#' @returns `"identical"` if `model_basic` and `model_augmented` have the same
#'   set of covariates. `"nesting"` if the covariates of `model_basic` are a
#'   subset of the covariates of `model_augmented`.
#'
#' @keywords internal
h_check_covar_nesting <- function(model_basic, model_augmented) {
  assert_class(model_basic, "mmrm_tmb_formula_parts")
  assert_class(model_augmented, "mmrm_tmb_formula_parts")

  basic_terms <- terms(model_basic[["model_formula"]])
  aug_terms <- terms(model_augmented[["model_formula"]])

  basic_factors <- attr(basic_terms, "factors")
  aug_factors <- attr(aug_terms, "factors")

  is_interaction_basic <- attr(basic_terms, "order") > 1
  is_interaction_aug <- attr(aug_terms, "order") > 1

  basic_non_interactions <- colnames(basic_factors)[!is_interaction_basic]
  aug_non_interactions <- colnames(aug_factors)[!is_interaction_aug]

  if (anyNA(match(basic_non_interactions, aug_non_interactions))) {
    stop("Each model's covariates must be a subset of the next model's ",
         "covariates.", call. = FALSE)
  }

  if (any(is_interaction_basic)) {
    interactions_factors_basic <-
      basic_factors[, is_interaction_basic, drop = FALSE]
    interactions_factors_aug <-
      aug_factors[rownames(basic_factors), is_interaction_aug, drop = FALSE]

    interaction_nesting <-
      h_many_to_many_interaction_nesting_test(
        interactions_factors_basic = interactions_factors_basic,
        interactions_factors_aug = interactions_factors_aug
      )

    if (any(interaction_nesting == "not_nested")) {
      stop("Interaction terms must be nested.", call. = FALSE)
    }
  } else {
    interaction_nesting <- character()
  }

  if (anyNA(match(aug_non_interactions, basic_non_interactions)) ||
      any(interaction_nesting == "nested")) {
    "nested"
  } else {
    "identical"
  }
}


h_one_to_one_interaction_nesting_test <- function(interaction_factors_basic,
                                                  interaction_factors_aug) {
  if (all(interaction_factors_basic == interaction_factors_aug)) {
    return(c(identical = 1L))
  }

  if (all(interaction_factors_basic <= interaction_factors_aug)) {
    return(c(nested = 2L))
  }

  c(not_nested = 3L)
}





h_one_to_many_interaction_nesting_test <- function(interaction_factors_basic,
                                                   interactions_factors_aug) {
  nesting_results <-
    apply(
      interactions_factors_aug,
      MARGIN = 2L,
      h_one_to_one_interaction_nesting_test,
      interaction_factors_basic = interaction_factors_basic,
      simplify = TRUE
    )
  min(nesting_results)
}


h_many_to_many_interaction_nesting_test <- function(interactions_factors_basic,
                                                    interactions_factors_aug) {
  i <-
    apply(
      interactions_factors_basic,
      MARGIN = 2L,
      h_one_to_many_interaction_nesting_test,
      interactions_factors_aug = interactions_factors_aug,
      simplify = TRUE
    )
  c("identical", "nested", "not_nested")[i]
}


#' Ensure Two Models' Covariance Structures Are Nested
#'
#' Throws an error if the covariance structure of `model_basic` isn't the same
#' as or a special case of the covariance structure of `model_augmented`.
#'
#' The check for "nesting" is a check against a mathematically determined
#' hierarchy of the available [cov_types]: if restricting an aspect of a
#' covariance structure can result in another covariance structure, the latter
#' structure is nested within the former structure.
#'
#' For upstream coding brevity, this function accepts the `formula_parts`
#' element of two `mmrm` model fits rather than `mmrm` objects. Such objects are
#' of class `mmrm_tmb_formula_parts`.
#'
#' @param model_basic,model_augmented (`mmrm_tmb_formula_parts`)\cr the
#'   `formula_parts` element of an `mmrm` model fit.
#'
#' @returns `"identical"` if `model_basic` and `model_augmented` have the same
#'   covariance structure. `"nesting"` if the covariance structure of
#'   `model_basic` is a special case of the covariance structure of
#'   `model_augmented`.
#'
#' @keywords internal
h_check_cov_struct_nesting <- function(model_basic, model_augmented) {
  assert_class(model_basic, "mmrm_tmb_formula_parts")
  assert_class(model_augmented, "mmrm_tmb_formula_parts")

  basic_cov_struct <- model_basic[["cov_type"]]
  aug_cov_struct <- model_augmented[["cov_type"]]

  # Hierarchy, specifying the "parent" covariance structures of each available
  # type in mmrm
  cov_struct_nesting <-
    list(
      ad = c("adh", "us"),
      adh = "us",
      ar1 = c("ad", "adh", "ar1h", "toep", "toeph", "us"),
      ar1h = c("adh", "toeph", "us"),
      cs = c("csh", "toep", "toeph", "us"),
      csh = c("toeph", "us"),
      toep = c("toeph", "us"),
      toeph = "us",
      us = character(0L),
      sp_exp = character(0L)
    )

  if (identical(basic_cov_struct, aug_cov_struct)) {
    "identical"
  } else if (any(cov_struct_nesting[[basic_cov_struct]] == aug_cov_struct)) {
    "nested"
  } else {
    stop("Each model's covariance structure must be either identical to or a ",
         "special case of the next model's covariance structure.",
         call. = FALSE)
  }
}






#' Generate a Name Not Already In an Environment nor Its Parents
#'
#' Alters the user-supplied string `x` using [make.names()] with `unique = TRUE`
#' until it is a syntactically valid name not bound to in `env` nor its
#' [parents][parent.env].
#'
#' @param x (`string`)\cr a candidate name.
#' @param env (`environment`)\cr an [environment] whose bindings (and whose
#'   parents' bindings) are checked to ensure that they do not include the
#'   returned value.
#'
#' @returns A string that does not match any of the bindings of `env` nor its
#'   parents.
#'
#' @keywords internal
h_generate_new_name <- function(x, env) {
  assert_string(x, na.ok = TRUE)
  assert_environment(env)

  # Force x to be a syntactically valid name.
  x <- make.names(x, unique = TRUE)

  # As long as we keep finding a binding in env (or its parents) whose name is
  # the same as the last element of x...
  while (exists(x[length(x)], envir = env, inherits = TRUE)) {
    # ...Copy the first element of x and append it to the end.
    # Run make.names(unique = TRUE) again.
    x <- make.names(x[c(seq_along(x), 1L)], unique = TRUE)
  }

  # If we've gotten here, the last element of doesn't exist in env nor its
  # parents
  x[length(x)]
}





#' Refit an `mmrm` Model Using a New Dataset
#'
#' Extract the `call` [component] of `fit` and evaluate it in a new
#' [environment][new.env] that contains the new `data`.
#'
#' This works as follows:
#'
#' 1. A new environment is created whose parent is
#' `environment(fit$formula_parts$full_formula)`.
#'
#' 1. A name is generated using [h_generate_new_name()], and `data` is bound to
#' the new environment using this new name.
#'
#' 1. The [`call`] component of `fit` is extracted and its `data` argument is
#' changed to the new name.
#'
#' 1. The modified call is evaluated in the new environment.
#'
#' @param fit (`mmrm`)\cr an `mmrm` object to be refit.
#' @param data (`data frame`)\cr a data frame upon which `fit` is to be refit.
#'
#' @returns An `mmrm` object with the same terms as `fit` but based on `data`.
#'
#' @keywords internal
h_refit_mmrm <- function(fit, data) {
  assert_class(fit, "mmrm")
  assert_data_frame(data)

  # Grab the environment of the model's formula.
  env <- environment(fit[["formula_parts"]][["full_formula"]])
  assert_environment(env)

  # Grab the model call.
  expr <- component(fit, "call")

  # Generate a name guaranteed not to be in env nor its enclosing frames
  data_name <- h_generate_new_name("data", env)

  # Create a child environment whose parent is the model's environment.
  env <- new.env(parent = env)

  # Bind the new data to the new name in the new, child environment.
  env[[data_name]] <- data

  # Force the data argument of the model call to be the new name.
  expr[["data"]] <- as.name(data_name)

  # Evaluate the updated model call in the new environment.
  fit <- eval(expr, env)

  fit
}




#' Calculate the Significance of Each Term in an `mmrm` Fit.
#'
#' Runs [df_md()] once for each term in an `mmrm` object and returns the results
#' in a data frame.
#'
#' When only one model fit is passed to [anova.mmrm()], the `object` argument of
#' `anova.mmrm()` is passed directly to this function.
#'
#' @param object (`mmrm`)\cr an `mmrm` object.
#'
#' @returns A data frame with a row for each term in `object`, including an
#'   intercept if present. The [row.names] of the data frame identify the terms.
#'   The data frame will contain the following four columns:
#'
#'  - `num_df`: the numerator degrees of freedom.
#'  - `denom_df`: the denominator degrees of freedom.
#'  - `f_stat`: the test statistic on the F-distribution.
#'  - `p_va`: the associated p-value.
#'
#' @keywords internal
h_anova_single_mmrm_model <- function(object) {
  assert_class(object, "mmrm")

  model_terms_attributes <-
    attributes(terms(object[["formula_parts"]][["model_formula"]]))

  term_labels <- c(if (model_terms_attributes[["intercept"]]) "(Intercept)",
                   model_terms_attributes[["term.labels"]])

  design_matrix <- component(object, "x_matrix")

  # Integer vector with one entry per model coefficient, giving the index of the
  # term that corresponds to the coefficient.
  assign_coef_term <- attr(design_matrix, "assign")

  assign_coef_term <- factor(assign_coef_term, labels = term_labels)

  coef_identity_matrix <- diag(length(assign_coef_term))

  contrast_matrix_per_term <-
    split.data.frame(coef_identity_matrix, f = assign_coef_term)

  df_md_results_per_term <-
    lapply(contrast_matrix_per_term, df_md, object = object)
  df_md_results_per_term <- lapply(df_md_results_per_term, as.data.frame)

  df_md_results_data_frame <- do.call(rbind, df_md_results_per_term)

  df_md_results_data_frame
}
