# Flatten Expressions for Non-standard Evaluation

Used primarily to support the parsing of covariance structure
definitions from formulas, these functions flatten the syntax tree into
a hierarchy-less grammar, allowing for parsing that doesn't abide by R's
native operator precedence.

## Usage

``` r
flatten_call(call)

flatten_expr(expr)
```

## Arguments

- call, expr:

  (`language`)  
  a language object to flatten.

## Value

A list of atomic values, symbols, infix operator names and
subexpressions.

## Details

Where `1 + 2 | 3` in R's syntax tree is `(|, (+, 1, 2), 3)`, flattening
it into its visual order produces `(1, +, 2, |, 3)`, which makes for
more fluent interpretation of non-standard grammar rules used in
formulas.

## Functions

- `flatten_call()`: Flatten a call into a list of names and argument
  expressions.

  The call name and all arguments are flattened into the same list,
  meaning a call of the form `sp_exp(a, b, c | d / e)` produces a list
  of the form `(sp_exp, a, b, c, |, d, /, e)`.

      flatten_call(quote(sp_exp(a, b, c | d / e)))

- `flatten_expr()`: Flatten nested expressions

      flatten_expr(quote(1 + 2 + 3 | 4))
