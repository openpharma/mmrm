# Capture all Output

This function silences all warnings, errors & messages and instead
returns a list containing the results (if it didn't error), as well as
the warnings, errors and messages and divergence signals as character
vectors.

## Usage

``` r
h_record_all_output(expr, remove = list(), divergence = list())
```

## Arguments

- expr:

  (`expression`)  
  to be executed.

- remove:

  (`list`)  
  optional list with elements `warnings`, `errors`, `messages` which can
  be character vectors, which will be removed from the results if
  specified.

- divergence:

  (`list`)  
  optional list similar as `remove`, but these character vectors will be
  moved to the `divergence` result and signal that the fit did not
  converge.

## Value

A list containing

- `result`: The object returned by `expr` or
  [`list()`](https://rdrr.io/r/base/list.html) if an error was thrown.

- `warnings`: `NULL` or a character vector if warnings were thrown.

- `errors`: `NULL` or a string if an error was thrown.

- `messages`: `NULL` or a character vector if messages were produced.

- `divergence`: `NULL` or a character vector if divergence messages were
  caught.
