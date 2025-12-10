# Quadratic Form Calculations

These helpers are mainly for easier readability and slightly better
efficiency of the quadratic forms used in the Satterthwaite
calculations.

## Usage

``` r
h_quad_form_vec(vec, center)

h_quad_form_mat(mat, center)
```

## Arguments

- vec:

  (`numeric`)  
  interpreted as a row vector.

- center:

  (`matrix`)  
  square numeric matrix with the same dimensions as `x` as the center of
  the quadratic form.

- mat:

  (`matrix`)  
  numeric matrix to be multiplied left and right of `center`, therefore
  needs to have as many columns as there are rows and columns in
  `center`.

## Functions

- `h_quad_form_vec()`: calculates the number `vec %*% center %*% t(vec)`
  as a numeric (not a matrix).

- `h_quad_form_mat()`: calculates the quadratic form
  `mat %*% center %*% t(mat)` as a matrix, the result is square and has
  dimensions identical to the number of rows in `mat`.
