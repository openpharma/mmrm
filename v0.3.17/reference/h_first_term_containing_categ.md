# Identify the First Term in a Model to Contain a Categorical Variable

This returns the column name of the leftmost column of `factors`
containing a nonzero value in a row corresponding to a `categorical`
variable.

## Usage

``` r
h_first_term_containing_categ(factors, categorical)
```

## Arguments

- factors:

  (matrix)  
  the `factors` attribute of a
  [`terms.object`](https://rdrr.io/r/stats/terms.object.html), which is
  a matrix of 0s, 1s, and 2s.

- categorical:

  (character)  
  a vector of the categorical variables in the model whose
  [`terms.object`](https://rdrr.io/r/stats/terms.object.html) is
  `factors`.

## Value

A `string`: one of the column names of `factors`. If none of the columns
contain a categorical variable, `NULL` is returned.
