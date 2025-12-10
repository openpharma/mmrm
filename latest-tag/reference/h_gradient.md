# Computation of a Gradient Given Jacobian and Contrast Vector

Computes the gradient of a linear combination of `beta` given the
Jacobian matrix and variance parameters.

## Usage

``` r
h_gradient(jac_list, contrast)
```

## Arguments

- jac_list:

  (`list`)  
  Jacobian list produced e.g. by
  [`h_jac_list()`](https://openpharma.github.io/mmrm/reference/h_jac_list.md).

- contrast:

  (`numeric`)  
  contrast vector, which needs to have the same number of elements as
  there are rows and columns in each element of `jac_list`.

## Value

Numeric vector which contains the quadratic forms of each element of
`jac_list` with the `contrast` vector.
