#' Search For the Position of a Symbol
#'
#' A thin wrapper around [base::Position()] to search through a list of language
#' objects, as produced by [flatten_call()] or [flatten_expr()], for the
#' presence of a specific symbol.
#'
#' @param x (`list` of `language`)\cr a list of language objects in which to
#'   search for a specific symbol.
#' @param sym (`name` or `symbol` or `character`)\cr a symbol to search for in
#'   `x`.
#' @param ... Additional arguments passed to `Position()`.
#'
#' @return The position of the symbol if found, or the `nomatch` value
#'   otherwise.
#'
#' @keywords internal
position_symbol <- function(x, sym, ...) {
  Position(function(i) identical(i, as.symbol(sym)), x, ...)
}

#' Flatten Expressions for Non-standard Evaluation
#'
#' Used primarily to support the parsing of covariance structure definitions
#' from formulas, these functions flatten the syntax tree into a hierarchy-less
#' grammar, allowing for parsing that doesn't abide by R's native operator
#' precedence.
#'
#' Where \code{1 + 2 | 3} in R's syntax tree is \code{(|, (+, 1, 2), 3)},
#' flattening it into its visual order produces \code{(1, +, 2, |, 3)}, which
#' makes for more fluent interpretation of non-standard grammar rules used in
#' formulas.
#'
#' @param call,expr (`language`)\cr a language object to flatten.
#'
#' @return A list of atomic values, symbols, infix operator names and
#'   subexpressions.
#'
#' @name flat_expr
#' @keywords internal
NULL

#' @describeIn flat_expr
#' Flatten a call into a list of names and argument expressions.
#'
#' The call name and all arguments are flattened into the same list, meaning a
#' call of the form \code{sp_exp(a, b, c | d / e)} produces a list of the form
#' \code{(sp_exp, a, b, c, |, d, /, e)}.
#'
#' ```
#' flatten_call(quote(sp_exp(a, b, c | d / e)))
#' ```
#'
#' @keywords internal
flatten_call <- function(call) {
  flattened_args <- unlist(lapply(call[-1], flatten_expr))
  c(flatten_expr(call[[1]]), flattened_args)
}

#' @describeIn flat_expr
#' Flatten nested expressions
#'
#' ```
#' flatten_expr(quote(1 + 2 + 3 | 4))
#' ```
#'
#' @keywords internal
flatten_expr <- function(expr) {
  if (length(expr) > 1 && is_infix(expr[[1]])) {
    op <- list(expr[[1]])
    lhs <- flatten_expr(expr[[2]])
    rhs <- flatten_expr(expr[[3]])
    c(lhs, op, rhs)
  } else {
    list(expr)
  }
}

#' Extract Right-Hand-Side (rhs) from Formula
#'
#' @param f (`formula`)\cr a formula.
#'
#' @return A formula without a response, derived from the right-hand-side of the
#'   formula, `f`.
#'
#' ```
#' formula_rhs(a ~ b + c)
#' formula_rhs(~ b + c)
#' ```
#'
#' @keywords internal
formula_rhs <- function(f) {
  if (length(f) == 2) {
    f
  } else {
    f[-2]
  }
}

#' Test Whether a Symbol is an Infix Operator
#'
#' @param name (`symbol` or `name` or `string`)\cr a possible reference to an
#'   infix operator to check.
#'
#' @return A logical indicating whether the name is the name of an infix
#'   operator.
#'
#' ```
#' is_infix(as.name("|"))
#' is_infix("|")
#' is_infix("c")
#' ```
#'
#' @keywords internal
is_infix <- function(name) {
  "Ops" %in% methods::getGroup(as.character(name), recursive = TRUE)
}

#' Format Symbol Objects
#'
#' For printing, variable names are converted to symbols and deparsed to use R's
#' built-in formatting of variables that may contain spaces or quote characters.
#'
#' @param x (`character`) A vector of variable names.
#'
#' @return A formatted string of comma-separated variables.
#'
#' @keywords internal
format_symbols <- function(x) {
  paste0(collapse = ", ", lapply(x, function(i) {
    utils::capture.output(as.symbol(i))
  }))
}
