#' Covariance Types
#'
#' An internal constant for covariance type lookup
#'
#' @keywords internal
COV_TYPES <- matrix(  # nolint
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c(), c("abbr", "abbr_hetero", "full")),
  c(
    "us",     NA,      "unstructured",
    "toep",   "toeph", "toeplitz",
    "ar1",    "ar1h",  "auto regressive",
    "ad",     "adh",   "ante dependence",
    "cs",     "csh",   "compound symmetry",
    "sp_exp", NA,      "spatial"
  )
)



#' Retrieve Available Covariance Structures
#'
#' @return A character vector of accepted covariance structure type names and
#'   abbreviations
#'
#' @family cov_struct
#' @export
cov_types <- function() {
  as.vector(t(COV_TYPES)[!is.na(t(COV_TYPES))])
}



#' Produce A Covariance Identifier Passing to TMB
#'
#' @param cov (`cov_struct`)\cr
#'   A covariance structure object
#'
#' @return A string used for method dispatch when passed to TMB
#'
#' @keywords internal
tmb_cov_type <- function(cov) {
  paste0(cov$type, if (cov$heterogeneous) "h")
}



#' Define a Covariance Structure
#'
#' @param type (`string`)\cr
#'   The name of the covariance structure type to use. For available options,
#'   see `cov_types()`. If a type abbreviation is used that implies
#'   heterogenicity (e.g. `cph`) and no value is provided to `heterogeneous`,
#'   then the heterogenicity is derived from the type name.
#' @param visits (`character`)\cr
#'   A vector of variable names to use for the longitudinal terms of the
#'   covariance structure. Multiple terms are only permitted for the `"spatial"`
#'   covariance type.
#' @param subject (`string`)\cr
#'   The name of the variable that encodes a subject identifier.
#' @param group (`string`)\cr
#'   Optionally, the name of the variable that encodes a grouping variable for
#'   subjects.
#'
#' @return A `cov_struct` object
#'
#' @examples
#' cov_struct("csh", "AVISITN", "USUBJID")
#' cov_struct("spatial", c("VISITA", "VISITB"), group = "GRP", subject = "SBJ")
#'
#' @family cov_struct
#' @export
cov_struct <- function(type = cov_types(), visits, subject, group = NULL,
  heterogeneous = FALSE) {

  # if heterogeneous isn't provided, derive from provided type
  if (missing(heterogeneous)) {
    heterogeneous <- switch(type,
      toeph = , ar1h = , adh = , csh = TRUE,
      heterogeneous
    )
  }

  # coerce all type options into abbreviated form
  type <- match.arg(type)
  type <- COV_TYPES[, 1][which(COV_TYPES == type, arr.ind = TRUE)[, 1]]

  x <- structure(
    list(
      type = type,
      heterogeneous = heterogeneous,
      visits = visits,
      subject = subject,
      group = group
    ),
    class = c("cov_struct", "mmrm_cov_struct", "list")
  )

  validate_cov_struct(x)
}



#' Validate Covariance Structure Data
#'
#' Run checks against relational integrity of covariance definition
#'
#' @param x (`cov_struct`)\cr
#'   A covariance structure object
#'
#' @return `x` if successful, or an error is thrown otherwise
#'
#' @keywords internal
validate_cov_struct <- function(x) {
  check("stop", length(x$subject) == 1,
    "Exactly one variable must be provided for the covariance structure's `subject`")

  check("stop", length(x$group) <= 1,
    "At most one variable is allowed for the covariance structure's `group`")

  check("stop", length(x$visits) > 0,
    "A variable must be provided for the covariance structure's `time`")

  check("stop", x$type == "sp_exp" || length(x$visits) < 2,
    "Multiple `time` variables are only permitted for 'spatial' type covariance")

  check("warn", !any(dup <- duplicated(x$visits)),
    "Duplicated `time` variable(s): ", fmt_syms(unique(x$visits[dup])))

  emit_check_results()
  x
}



#' Format Covariance Structure Object
#'
#' @param x (`cov_struct`)\cr
#'   A covariance structure object
#' @param ...
#'   Additional arguments unused
#'
#' @return A formatted string for `x`
#'
#' @export
format.cov_struct <- function(x, ...) {
  long_name <- COV_TYPES[, 3][which(COV_TYPES == x$type, arr.ind = TRUE)[, 1]]
  sprintf("<covariance structure>\n%s%s:\n\n  %s | %s%s\n",
    long_name,
    if (x$heterogeneous) " (heterogeneous)" else "",
    fmt_syms(x$visits),
    if (!is.null(x$group)) paste0(fmt_syms(x$group), " / ") else "",
    fmt_syms(x$subject)
  )
}



#' Print a Covariance Structure Object
#'
#' @param x (`cov_struct`)\cr
#'   A covariance structure object
#' @param ...
#'   Additional arguments unused
#'
#' @return `x` invisibly
#'
#' @export
print.cov_struct <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}



#' Coerce into a Covariance Structure Definition
#'
#' @details
#' A covariance structure can be parsed from a model definition formula or call.
#' Generally, covariance structures defined using non-standard evaluation take
#' the following form:
#'
#' ```
#' type( (visit, )* visit | (group /)? subject )
#' ```
#'
#' For example, formulas may include terms such as
#'
#' ```r
#' us(time | subject)
#' cp(time | group / subject)
#' sp_exp(time1, time2 | group / subject)
#' ```
#'
#' Note that only `sp_exp` (spatial) covariance structures may provide multiple
#' longitudinal variables.
#'
#' @param x An object from which to derive a covariance structure. See object
#'   specific sections for details.
#' @param warn_partial (`bool`)\cr
#'   Whether to emit a warning when parts of the formula are disregarded.
#'
#' @return A `cov_struct` object
#'
#' @examples
#' # provide a covariance structure as a right-sided formula
#' as.cov_struct( ~ csh(visit | group / subject) )
#'
#' # when part of a model, suppress warnings using `warn_partial = FALSE`
#' as.cov_struct( y ~ x + csh(visit | group / subject), warn_partial = FALSE)
#'
#' # provide a quoted expression
#' as.cov_struct(quote(sp_exp(visitA, visitB | group / subject)))
#'
#' @family cov_struct
#' @export
as.cov_struct <- function(x, ...) {  # nolint
  UseMethod("as.cov_struct")
}



#' @export
as.cov_struct.cov_struct <- function(x, ...) {
  x
}



#' @describeIn as.cov_struct
#' When provided a formula, any specialized functions are assumed to be
#' covariance structure defintions and must follow the form:
#'
#' ```
#' y ~ xs + type( (visit, )* visit | (group /)? subject )
#' ```
#'
#' Any component on the right hand side of a formula is considered when
#' searching for a covariance definition.
#'
#' @export
as.cov_struct.formula <- function(x, ...) {
  as.cov_struct(formula_rhs(x), ...)
}



#' @describeIn as.cov_struct
#' Calls may be provided directly to `as.cov_struct`, but are more likely used
#' internally to process the right-hand side of a formula.
#'
#' @export
as.cov_struct.call <- function(x, warn_partial = TRUE) {
  x_calls <- expr_cov_struct_calls(x)

  if (warn_partial && !identical(x, x_calls[[1]])) {
    warning("Formula contains unrecognized covariance structure terms.")
  }

  if (length(x_calls) > 1) {
    cov_struct_types <- as.character(lapply(x_calls, `[[`, , 1L))
    stop(
      "Ambiguous covariance structure. Found multiple calls in formula: ",
      paste0('"', cov_struct_types, '"', collapse = ", ")
    )
  }

  # flatten into list of infix operators, calls and names/atomics
  x <- x_calls[[1]]
  type <- as.character(x[[1]])
  x <- unlist(lapply(x[-1], flatten_expr))

  # take visits until "|"
  i <- position_symbol(x, "|", nomatch = FALSE)
  if (!i) stop("Covariance structure requires form `visit | ..`")
  visits <- as.character(head(x, i - 1))
  if (i) x <- x[-seq(to = i)]

  # take group until "/"
  i <- position_symbol(x, "/", nomatch = FALSE)
  group <- if (!i) NULL else as.character(head(x, i - 1))
  if (i) x <- x[-seq(to = i)]

  # remainder is subject
  subject <- as.character(x)

  cov_struct(type = type, visits = visits, group = group, subject = subject)
}
