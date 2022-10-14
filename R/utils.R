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
#' @description `r lifecycle::badge("experimental")`
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
