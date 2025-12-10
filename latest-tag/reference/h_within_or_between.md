# Determine Within or Between for each Design Matrix Column

Used in
[`h_df_bw_calc()`](https://openpharma.github.io/mmrm/reference/h_df_bw_calc.md)
to determine whether a variable differs only between subjects or also
within subjects.

## Usage

``` r
h_within_or_between(x_matrix, subject_ids)
```

## Arguments

- x_matrix:

  (`matrix`)  
  the design matrix with column names.

- subject_ids:

  (`factor`)  
  the subject IDs.

## Value

Character vector with "intercept", "within" or "between" for each design
matrix column identified via the names of the vector.
