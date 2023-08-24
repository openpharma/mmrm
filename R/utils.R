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
  assert_list(remove)
  env <- new.env()
  result <- withCallingHandlers(
    withRestarts(
      expr,
      muffleStop = function() list()
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
      invokeRestart("muffleStop")
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
    ret <- modifyList(control, list(...))
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
    args = modifyList(list(
      .Data = fun, args = modifyList(args, a_args),
      class = c("partial", "function")
    ), additional_attr)
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
  return(TRUE)
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

#' Convert Character to Factor Following Reference
#'
#' @param x (`character` or `factor`)\cr  input.
#' @param ref (`factor`)\cr reference.
#' @param var_name (`string`)\cr variable name of input `x`.
#'
#' @details Use `ref` to convert `x` into factor with the same levels.
#' This is needed even if `x` and `ref` are both `character` because
#' in `model.matrix` if `x` only has one level there could be errors.
#'
#' @keywords internal
h_factor_ref <- function(x, ref, var_name = vname(x)) {
  assert_multi_class(ref, c("character", "factor"))
  assert_multi_class(x, c("character", "factor"))
  # NA can be possible values
  uni_values <- as.character(stats::na.omit(unique(x)))
  # no NA in reference
  uni_ref <- as.character(unique(ref))
  assert_character(uni_values, .var.name = var_name)
  assert_subset(uni_values, uni_ref, .var.name = var_name)
  factor(x, levels = h_default_value(levels(ref), sort(uni_ref)))
}

#' Warn on na.action
#' @keywords internal
h_warn_na_action <- function() {
  if (!identical(getOption("na.action"), "na.omit")) {
    warning("na.action is always set to `na.omit` for `mmrm` fit!")
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
