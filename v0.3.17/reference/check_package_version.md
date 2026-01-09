# Check Suggested Dependency Against Version Requirements

Check Suggested Dependency Against Version Requirements

## Usage

``` r
check_package_version(pkg, ver = c(NA_character_, NA_character_))
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

## Value

A logical (invisibly) indicating whether the loaded package meets the
version requirements. A warning is emitted otherwise.
