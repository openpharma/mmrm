# Helper Function for Registering Functionality With Suggests Packages

Helper Function for Registering Functionality With Suggests Packages

## Usage

``` r
register_on_load(
  pkg,
  ver = c(NA_character_, NA_character_),
  callback,
  message = NULL
)
```

## Arguments

- pkg:

  (`string`)  
  package name.

- ver:

  (`character`)  
  of length 2 whose elements can be provided to
  [`numeric_version()`](https://rdrr.io/r/base/numeric_version.html),
  representing a minimum and maximum (inclusive) version requirement for
  interoperability. When `NA`, no version requirement is imposed.
  Defaults to no version requirement.

- callback:

  (`function(...) ANY`)  
  a callback to execute upon package load. Note that no arguments are
  passed to this function. Any necessary data must be provided upon
  construction.

- message:

  (`NULL` or `string`)  
  an optional message to print after the callback is executed upon
  successful registration.

## Value

A logical (invisibly) indicating whether registration was successful. If
not, a onLoad hook was set for the next time the package is loaded.
