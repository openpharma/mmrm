# Generate a Name Not Already In an Environment nor Its Parents

Alters the user-supplied string `x` using
[`make.names()`](https://rdrr.io/r/base/make.names.html) with
`unique = TRUE` until it is a syntactically valid name not bound to in
`env` nor its [parents](https://rdrr.io/r/base/environment.html).

## Usage

``` r
h_generate_new_name(x, env)
```

## Arguments

- x:

  (`string`)  
  a candidate name.

- env:

  (`environment`)  
  an [environment](https://rdrr.io/r/base/environment.html) whose
  bindings (and whose parents' bindings) are checked to ensure that they
  do not include the returned value.

## Value

A string that does not match any of the bindings of `env` nor its
parents.
