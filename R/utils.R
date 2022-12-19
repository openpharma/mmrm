#' Capture all Output
#'
#' This function silences all warnings, errors & messages and instead returns a list
#' containing the results (if it didn't error) + the warning and error messages as
#' character vectors.
#'
#' @param expr (`expression`)\cr to be executed.
#' @param remove (`list`)\cr optional list with elements `warnings`, `errors`,
#'   `messages` which can be character vectors, which will be removed from the
#'   results if specified.
#'
#' @return
#' A list containing
#'
#' - `result`: The object returned by `expr` or `list()` if an error was thrown.
#' - `warnings`: `NULL` or a character vector if warnings were thrown.
#' - `errors`: `NULL` or a string if an error was thrown.
#' - `messages`: `NULL` or a character vector if messages were produced.
#'
#' @keywords internal
h_record_all_output <- function(expr, remove = list()) {
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
    warnings = setdiff(env$warning, remove$warnings),
    errors = setdiff(env$error, remove$errors),
    messages = setdiff(env$message, remove$messages)
  )
}

#' Get an approximate number of free cores.
#'
#' @description `r lifecycle::badge("deprecated")` use
#' `parallelly::availableCores(omit = 1)` instead
#'
#' @return The approximate number of free cores, which is an integer between 1 and one less than
#' the total cores.
#'
#' @details
#' - This uses the maximum load average at 1, 5 and 15 minutes on Linux and Mac
#' machines to approximate the number of busy cores. For Windows, the load percentage is
#' multiplied with the total number of cores.
#' - We then subtract this from the number of all detected cores. One additional core
#' is not used for extra safety.
#'
#' @note If executed during a unit test and on CRAN then always returns 1 to avoid any
#' parallelization.
#'
#' @export
free_cores <- function() {
  lifecycle::deprecate_warn(
    "0.1.6", "free_cores()",
    "parallelly::availableCores(omit = 'number of cores to reserve, e.g. 1')"
  )
  all_cores <- parallel::detectCores(all.tests = TRUE)
  busy_cores <-
    if (.Platform$OS.type == "windows") {
      load_percent_string <- system("wmic cpu get loadpercentage", intern = TRUE)
      # This gives e.g.: c("LoadPercentage", "10", "")
      # So we just take the number here.
      load_percent <- as.integer(min(load_percent_string[2L], 100))
      if (test_int(load_percent, lower = 0, upper = 100)) {
        ceiling(all_cores * load_percent / 100)
      } else {
        all_cores
      }
    } else if (.Platform$OS.type == "unix") {
      uptime_string <- system("uptime", intern = TRUE)
      # This gives e.g.:
      # "11:00  up  1:57, 3 users, load averages: 2.71 2.64 2.62"
      # Here we just want the last three numbers.
      uptime_split <- strsplit(uptime_string, split = ",|\\s")[[1]] # Split at comma or white space.
      uptime_split <- uptime_split[uptime_split != ""]
      load_averages <- as.numeric(utils::tail(uptime_split, 3))
      ceiling(max(load_averages))
    }
  assert_number(all_cores, lower = 1, finite = TRUE)
  assert_number(busy_cores, lower = 0, upper = all_cores)
  # For safety, we subtract 1 more core from all cores.
  as.integer(max(1, all_cores - busy_cores - 1))
}

# covariance types ----

# nolint start

#' covariance type
#'
#' @format vector of supported covariance structures. `cov_type` for common time points covariance structures,
#' `cov_type_spatial` for spatial covariance structures.
#' @details
#' abbreviation for covariance structures
#' ## Common Covariance Structures
#'
#' | **Structure**     | **Description**                       | **Parameters**      | **\eqn{(i, j)} element**         |
#' | ------------- |-------------------------------------------|:---------------|----------------------------------|
#' | ad            | Ante-dependence                           | \eqn{m}        | \eqn{\sigma^{2}\prod_{k=i}^{j-1}\rho_{k}} |
#' | adh           | Heterogeneous ante-dependence             | \eqn{2m-1}     | \eqn{\sigma_{i}\sigma_{j}\prod_{k=i}^{j-1}\rho_{k}} |
#' | ar1           | First-order auto-regressive               | \eqn{2}        | \eqn{\sigma^{2}\rho^{\left \vert {i-j} \right \vert}} |
#' | ar1h          | Heterogeneous first-order auto-regressive | \eqn{m+1}      | \eqn{\sigma_{i}\sigma_{j}\rho^{\left \vert {i-j} \right \vert}} |
#' | cs            | Compound symmetry                         | \eqn{2}        | \eqn{\sigma^{2}\left[ \rho I(i \neq j)+I(i=j) \right]} |
#' | csh           | Heterogeneous compound symmetry           | \eqn{m+1}      | \eqn{\sigma_{i}\sigma_{j}\left[ \rho I(i \neq j)+I(i=j) \right]} |
#' | toep          | Toeplitz                                  | \eqn{m}        | \eqn{\sigma_{\left \vert {i-j} \right \vert +1}} |
#' | toeph         | Heterogeneous Toeplitz                    | \eqn{2m-1}     | \eqn{\sigma_{i}\sigma_{j}\rho_{\left \vert {i-j} \right \vert}} |
#' | us            | Unstructured                              | \eqn{m(m+1)/2} | \eqn{\sigma_{ij}} |
#'
#' where \eqn{i} and \eqn{j} denote \eqn{i}-th and \eqn{j}-th time points, respectively, out of total \eqn{m} time points, \eqn{1 \leq i, j \leq m}.
#'
#' Note the **ante-dependence** covariance structure in this package refers to homogeneous ante-dependence, while the ante-dependence covariance structure from SAS `PROC MIXED` refers to heterogeneous ante-dependence and the homogeneous version is not available in SAS.
#'
#' ## Spatial Covariance structures
#'
#' | **Structure**     | **Description**                       | **Parameters**      | **\eqn{(i, j)} element**         |
#' | ------------- |-------------------------------------------|:---------------|----------------------------------|
#' | sp_exp        | spatial exponential                       | \eqn{2}        | \eqn{\sigma^{2}\rho^{-d_{ij}}} |
#'
#' where \eqn{d_{ij}} denotes the Euclidean distance between time points \eqn{i} and \eqn{j}.
#' @md
#' @name covariance_types
NULL

# nolint end

#' @describeIn covariance_types non-spatial covariance structure
#' @format NULL
cov_type <- c("us", "toep", "toeph", "ar1", "ar1h", "ad", "adh", "cs", "csh")
#' @describeIn covariance_types spatial covariance structure
#' @format NULL
cov_type_spatial <- c("sp_exp")


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
  sum(diag(x))
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
