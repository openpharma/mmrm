# Search For the Position of a Symbol

A thin wrapper around
[`base::Position()`](https://rdrr.io/r/base/funprog.html) to search
through a list of language objects, as produced by
[`flatten_call()`](https://openpharma.github.io/mmrm/reference/flat_expr.md)
or
[`flatten_expr()`](https://openpharma.github.io/mmrm/reference/flat_expr.md),
for the presence of a specific symbol.

## Usage

``` r
position_symbol(x, sym, ...)
```

## Arguments

- x:

  (`list` of `language`)  
  a list of language objects in which to search for a specific symbol.

- sym:

  (`name` or `symbol` or `character`)  
  a symbol to search for in `x`.

- ...:

  Additional arguments passed to
  [`Position()`](https://rdrr.io/r/base/funprog.html).

## Value

The position of the symbol if found, or the `nomatch` value otherwise.
