# Split Control List

Split the
[`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md)
object according to its optimizers and use additional arguments to
replace the elements in the original object.

## Usage

``` r
h_split_control(control, ...)
```

## Arguments

- control:

  (`mmrm_control`)  
  object.

- ...:

  additional parameters to update the `control` object.

## Value

A `list` of `mmrm_control` entries.
