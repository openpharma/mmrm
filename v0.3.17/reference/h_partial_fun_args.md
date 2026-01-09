# Create Partial Functions

Creates partial functions with arguments.

## Usage

``` r
h_partial_fun_args(fun, ..., additional_attr = list())
```

## Arguments

- fun:

  (`function`)  
  to be wrapped.

- ...:

  Additional arguments for `fun`.

- additional_attr:

  (`list`)  
  of additional attributes to apply to the result.

## Value

Object with S3 class `"partial"`, a `function` with `args` attribute
(and possibly more attributes from `additional_attr`).

## Details

This function add `args` attribute to the original function, and add an
extra class `partial` to the function. `args` is the argument for the
function, and elements in `...` will override the existing arguments in
attribute `args`. `additional_attr` will override the existing
attributes.
