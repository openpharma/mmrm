#' Search For the Position of a Symbol
#'
#' A thin wrapper around `Position()` to search through a list of language
#' objects, as produced by `flatten_expr()`, for the presence of a specific
#' symbol.
#'
#' @param x (`list` of `language`)\cr
#'   A list of language objects in which to search for a specific symbol.
#' @param sym (`name` or `symbol` or `character`)\cr
#'   A symbol to search for in `x`
#' @param ... Additional arguments passed to `Position()`
#'
#' @return The position of the symbol if found, or the `nomatch` value
#'   otherwise.
#'
#' @keywords internal
position_symbol <- function(x, sym, ...) {
  Position(function(i) identical(i, as.symbol(sym)), x, ...)
}



#' Flatten Nested Binary Expressions into a List of Expressions and Operators
#'
#' Used primarily to support the parsing of covariance structure definitions
#' from formulas, this function flattens the syntax tree into a hierarchy-less
#' grammar, allowing for parsing that doesn't abide by R's native operator
#' precedence.
#'
#' @param expr (`language`)\cr
#'   A language object to flatten.
#'
#' @return A list of atomic values, symbols, infix operator names and
#'   subexpressions.
#'
#' @examples
#' flatten_expr(quote(1 + 2 + 3 | 4))
#'
#' @keywords internal
flatten_expr <- function(expr) {
  if (length(expr) == 1) {
    list(expr)
  } else if (is_infix(expr[[1]])) {
    lhs <- flatten_expr(expr[[2]])
    op  <- list(expr[[1]])
    rhs <- flatten_expr(expr[[3]])
    append(lhs, append(op, rhs))
  } else {
    list(expr)
  }
}



#' Extract Right-Hand-Side (rhs) from Formula
#'
#' @param f (`formula`)\cr
#'   A formula
#'
#' @return A quote or atomic value derived from the right-hand-side of the
#'   formula
#'
#' @examples
#' formula_rhs(a ~ b + c)
#' formula_rhs(~ b + c)
#'
#' @keywords internal
formula_rhs <- function(f) {
  f[[length(f)]]
}



#' Test Whether a Symbol is an Infix Operator
#'
#' @param name (`symbol` or `name` or `string`)
#'   A possible reference to an infix operator to check.
#'
#' @return A logical indicating whether the name is the name of an infix
#'   operator.
#'
#' @examples
#' is_infix(as.name("|"))
#' is_infix("|")
#' is_infix("c")
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
#' @param x (`character`)
#'   A vector of variable names
#'
#' @return A formatted string of comma-separated variables
#'
#' @keywords internal
fmt_syms <- function(x) {
  paste0(lapply(x, function(i) capture.output(as.symbol(i))), collapse = ", ")
}
