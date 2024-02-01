#' Covariance Type Database
#'
#' An internal constant for covariance type information.
#'
#' @format A data frame with 5 variables and one record per covariance type:
#'
#' \describe{
#'   \item{name}{
#'     The long-form name of the covariance structure type
#'   }
#'   \item{abbr}{
#'     The abbreviated name of the covariance structure type
#'   }
#'   \item{habbr}{
#'     The abbreviated name of the heterogeneous version of a covariance
#'     structure type (The abbreviated name (`abbr`) with a trailing `"h"` if
#'     the structure has a heterogeneous implementation or `NA` otherwise).
#'   }
#'   \item{heterogeneous}{
#'     A logical value indicating whether the covariance structure has a
#'     heterogeneous counterpart.
#'   }
#'   \item{spatial}{
#'     A logical value indicating whether the covariance structure is spatial.
#'   }
#' }
#'
#' @keywords internal
COV_TYPES <- local({ # nolint
  type <- function(name, abbr, habbr, heterogeneous, spatial) {
    args <- as.list(match.call()[-1])
    do.call(data.frame, args)
  }

  as.data.frame(
    col.names = names(formals(type)),
    rbind(
      type("unstructured", "us", NA, FALSE, FALSE),
      type("Toeplitz", "toep", "toeph", TRUE, FALSE),
      type("auto-regressive order one", "ar1", "ar1h", TRUE, FALSE),
      type("ante-dependence", "ad", "adh", TRUE, FALSE),
      type("compound symmetry", "cs", "csh", TRUE, FALSE),
      type("spatial exponential", "sp_exp", NA, FALSE, TRUE)
    )
  )
})

#' Covariance Types
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param form (`character`)\cr covariance structure type name form. One or
#'   more of `"name"`, `"abbr"` (abbreviation), or `"habbr"` (heterogeneous
#'   abbreviation).
#' @param filter (`character`)\cr covariance structure type filter. One or
#'   more of `"heterogeneous"` or `"spatial"`.
#'
#' @return A character vector of accepted covariance structure type names and
#'   abbreviations.
#'
#' @section Abbreviations for Covariance Structures:
#'
#' ## Common Covariance Structures:
#'
#' \tabular{clll}{
#'
#' \strong{Structure}
#' \tab \strong{Description}
#' \tab \strong{Parameters}
#' \tab \strong{\eqn{(i, j)} element}
#' \cr
#'
#' ad
#' \tab Ante-dependence
#' \tab \eqn{m}
#' \tab \eqn{\sigma^{2}\prod_{k=i}^{j-1}\rho_{k}}
#' \cr
#'
#' adh
#' \tab Heterogeneous ante-dependence
#' \tab \eqn{2m-1}
#' \tab \eqn{\sigma_{i}\sigma_{j}\prod_{k=i}^{j-1}\rho_{k}}
#' \cr
#'
#' ar1
#' \tab First-order auto-regressive
#' \tab \eqn{2}
#' \tab \eqn{\sigma^{2}\rho^{\left \vert {i-j} \right \vert}}
#' \cr
#'
#' ar1h
#' \tab Heterogeneous first-order auto-regressive
#' \tab \eqn{m+1}
#' \tab \eqn{\sigma_{i}\sigma_{j}\rho^{\left \vert {i-j} \right \vert}}
#' \cr
#'
#' cs
#' \tab Compound symmetry
#' \tab \eqn{2}
#' \tab \eqn{\sigma^{2}\left[ \rho I(i \neq j)+I(i=j) \right]}
#' \cr
#'
#' csh
#' \tab Heterogeneous compound symmetry
#' \tab \eqn{m+1}
#' \tab \eqn{\sigma_{i}\sigma_{j}\left[ \rho I(i \neq j)+I(i=j) \right]}
#' \cr
#'
#' toep
#' \tab Toeplitz
#' \tab \eqn{m}
#' \tab \eqn{\sigma_{\left \vert {i-j} \right \vert +1}}
#' \cr
#'
#' toeph
#' \tab Heterogeneous Toeplitz
#' \tab \eqn{2m-1}
#' \tab \eqn{\sigma_{i}\sigma_{j}\rho_{\left \vert {i-j} \right \vert}}
#' \cr
#'
#' us
#' \tab Unstructured
#' \tab \eqn{m(m+1)/2}
#' \tab \eqn{\sigma_{ij}}
#'
#' }
#'
#' where \eqn{i} and \eqn{j} denote \eqn{i}-th and \eqn{j}-th time points,
#' respectively, out of total \eqn{m} time points, \eqn{1 \leq i, j \leq m}.
#'
#' @note The **ante-dependence** covariance structure in this package refers to
#' homogeneous ante-dependence, while the ante-dependence covariance structure
#' from SAS `PROC MIXED` refers to heterogeneous ante-dependence and the
#' homogeneous version is not available in SAS.
#'
#' @note For all non-spatial covariance structures, the time variable must
#' be coded as a factor.
#'
#' ## Spatial Covariance structures:
#'
#' \tabular{clll}{
#'
#' \strong{Structure}
#' \tab \strong{Description}
#' \tab \strong{Parameters}
#' \tab \strong{\eqn{(i, j)} element}
#' \cr
#'
#' sp_exp
#' \tab spatial exponential
#' \tab \eqn{2}
#' \tab \eqn{\sigma^{2}\rho^{-d_{ij}}}
#'
#' }
#'
#' where \eqn{d_{ij}} denotes the Euclidean distance between time points
#' \eqn{i} and \eqn{j}.
#'
#' @family covariance types
#' @name covariance_types
#' @export
cov_types <- function(
    form = c("name", "abbr", "habbr"),
    filter = c("heterogeneous", "spatial")) {
  form <- match.arg(form, several.ok = TRUE)
  filter <- if (missing(filter)) c() else match.arg(filter, several.ok = TRUE)
  df <- COV_TYPES[form][rowSums(!COV_TYPES[filter]) == 0, ]
  Filter(Negate(is.na), unlist(t(df), use.names = FALSE))
}

#' Retrieve Associated Abbreviated Covariance Structure Type Name
#'
#' @param type (`string`)\cr either a full name or abbreviate covariance
#'   structure type name to collapse into an abbreviated type.
#'
#' @return The corresponding abbreviated covariance type name.
#'
#' @keywords internal
cov_type_abbr <- function(type) {
  row <- which(COV_TYPES == type, arr.ind = TRUE)[, 1]
  COV_TYPES$abbr[row]
}

#' Retrieve Associated Full Covariance Structure Type Name
#'
#' @param type (`string`)\cr either a full name or abbreviate covariance
#'   structure type name to convert to a long-form type.
#'
#' @return The corresponding abbreviated covariance type name.
#'
#' @keywords internal
cov_type_name <- function(type) {
  row <- which(COV_TYPES == type, arr.ind = TRUE)[, 1]
  COV_TYPES$name[row]
}

#' Produce A Covariance Identifier Passing to TMB
#'
#' @param cov (`cov_struct`)\cr a covariance structure object.
#'
#' @return A string used for method dispatch when passed to TMB.
#'
#' @keywords internal
tmb_cov_type <- function(cov) {
  paste0(cov$type, if (cov$heterogeneous) "h")
}

#' Define a Covariance Structure
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param type (`string`)\cr the name of the covariance structure type to use.
#'   For available options, see `cov_types()`. If a type abbreviation is used
#'   that implies heterogeneity (e.g. `cph`) and no value is provided to
#'   `heterogeneous`, then the heterogeneity is derived from the type name.
#' @param visits (`character`)\cr a vector of variable names to use for the
#'   longitudinal terms of the covariance structure. Multiple terms are only
#'   permitted for the `"spatial"` covariance type.
#' @param subject (`string`)\cr the name of the variable that encodes a subject
#'   identifier.
#' @param group (`string`)\cr optionally, the name of the variable that encodes
#'   a grouping variable for subjects.
#' @param heterogeneous (`flag`)\cr
#'
#' @return A `cov_struct` object.
#'
#' @examples
#' cov_struct("csh", "AVISITN", "USUBJID")
#' cov_struct("spatial", c("VISITA", "VISITB"), group = "GRP", subject = "SBJ")
#'
#' @family covariance types
#' @export
cov_struct <- function(
    type = cov_types(), visits, subject, group = character(),
    heterogeneous = FALSE) {
  # if heterogeneous isn't provided, derive from provided type
  if (missing(heterogeneous)) {
    heterogeneous <- switch(type,
      toeph = ,
      ar1h = ,
      adh = ,
      csh = TRUE,
      heterogeneous
    )
  }

  # coerce all type options into abbreviated form
  type <- match.arg(type)
  type <- cov_type_abbr(type)

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

#' Reconcile Possible Covariance Structure Inputs
#'
#' @inheritParams mmrm
#'
#' @return The value `covariance` if it's provided or a covariance structure
#'   derived from the provided `formula` otherwise. An error is raised of both
#'   are provided.
#'
#' @keywords internal
h_reconcile_cov_struct <- function(formula = NULL, covariance = NULL) {
  assert_multi_class(covariance, c("formula", "cov_struct"), null.ok = TRUE)
  assert_formula(formula, null.ok = FALSE)
  if (inherits(covariance, "formula")) {
    covariance <- as.cov_struct(covariance)
  }
  if (!is.null(covariance) && length(h_extract_covariance_terms(formula)) > 0) {
    stop(paste0(
      "Redundant covariance structure definition in `formula` and ",
      "`covariance` arguments"
    ))
  }

  if (!is.null(covariance)) {
    return(covariance)
  }

  as.cov_struct(formula, warn_partial = FALSE)
}

#' Validate Covariance Structure Data
#'
#' Run checks against relational integrity of covariance definition
#'
#' @param x (`cov_struct`)\cr a covariance structure object.
#'
#' @return `x` if successful, or an error is thrown otherwise.
#'
#' @keywords internal
validate_cov_struct <- function(x) {
  checks <- checkmate::makeAssertCollection()

  with(x, {
    assert_character(subject, len = 1, add = checks)
    assert_logical(heterogeneous, len = 1, add = checks)

    if (length(group) > 1 || length(visits) < 1) {
      checks$push(
        "Covariance structure must be of the form `time | (group /) subject`"
      )
    }

    if (!type %in% cov_types(filter = "spatial") && length(visits) > 1) {
      checks$push(paste(
        "Non-spatial covariance structures must have a single longitudinal",
        "variable"
      ))
    }
  })

  reportAssertions(checks)
  x
}

#' Format Covariance Structure Object
#'
#' @param x (`cov_struct`)\cr a covariance structure object.
#' @param ... Additional arguments unused.
#'
#' @return A formatted string for `x`.
#'
#' @export
format.cov_struct <- function(x, ...) {
  sprintf(
    "<covariance structure>\n%s%s:\n\n  %s | %s%s\n",
    if (x$heterogeneous) "heterogeneous " else "",
    cov_type_name(x$type),
    format_symbols(x$visits),
    if (length(x$group) > 0) paste0(format_symbols(x$group), " / ") else "",
    format_symbols(x$subject)
  )
}

#' Print a Covariance Structure Object
#'
#' @param x (`cov_struct`)\cr a covariance structure object.
#' @param ... Additional arguments unused.
#'
#' @return `x` invisibly.
#'
#' @export
print.cov_struct <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' Coerce into a Covariance Structure Definition
#'
#' @description `r lifecycle::badge("maturing")`
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
#' sp_exp(coord1, coord2 | group / subject)
#' ```
#'
#' Note that only `sp_exp` (spatial) covariance structures may provide multiple
#' coordinates, which identify the Euclidean distance between the time points.
#'
#' @param x an object from which to derive a covariance structure. See object
#'   specific sections for details.
#' @param warn_partial (`flag`)\cr whether to emit a warning when parts of the
#'   formula are disregarded.
#' @param ... additional arguments unused.
#'
#' @return A [cov_struct()] object.
#'
#' @examples
#' # provide a covariance structure as a right-sided formula
#' as.cov_struct(~ csh(visit | group / subject))
#'
#' # when part of a full formula, suppress warnings using `warn_partial = FALSE`
#' as.cov_struct(y ~ x + csh(visit | group / subject), warn_partial = FALSE)
#'
#' @family covariance types
#' @export
as.cov_struct <- function(x, ...) { # nolint
  UseMethod("as.cov_struct")
}

#' @export
as.cov_struct.cov_struct <- function(x, ...) {
  x
}

#' @describeIn as.cov_struct
#' When provided a formula, any specialized functions are assumed to be
#' covariance structure definitions and must follow the form:
#'
#' ```
#' y ~ xs + type( (visit, )* visit | (group /)? subject )
#' ```
#'
#' Any component on the right hand side of a formula is considered when
#' searching for a covariance definition.
#'
#' @export
as.cov_struct.formula <- function(x, warn_partial = TRUE, ...) {
  x_calls <- h_extract_covariance_terms(x)

  if (length(x_calls) < 1) {
    stop(
      "Covariance structure must be specified in formula. ",
      "Possible covariance structures include: ",
      paste0(cov_types(c("abbr", "habbr")), collapse = ", ")
    )
  }

  if (length(x_calls) > 1) {
    cov_struct_types <- as.character(lapply(x_calls, `[[`, 1L))
    stop(
      "Only one covariance structure can be specified. ",
      "Currently specified covariance structures are: ",
      paste0(cov_struct_types, collapse = ", ")
    )
  }

  # flatten into list of infix operators, calls and names/atomics
  x <- flatten_call(x_calls[[1]])
  type <- as.character(x[[1]])
  x <- drop_elements(x, 1)

  # take visits until "|"
  n <- position_symbol(x, "|", nomatch = 0)
  visits <- as.character(utils::head(x, max(n - 1, 0)))
  x <- drop_elements(x, n)

  # take group until "/"
  n <- position_symbol(x, "/", nomatch = 0)
  group <- as.character(utils::head(x, max(n - 1, 0)))
  x <- drop_elements(x, n)

  # remainder is subject
  subject <- as.character(x)

  cov_struct(type = type, visits = visits, group = group, subject = subject)
}
