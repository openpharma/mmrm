# Predicate Indicating Whether Two Datasets Contain the Same Observations

Checks whether two datasets contain the same observations and that the
first dataset's columns are a subset of the second dataset's columns.

## Usage

``` r
h_check_columns_nested(data_basic, data_augmented)
```

## Arguments

- data_basic, data_augmented:

  (`data.frame`)  
  data frames to be compared.

## Value

`TRUE` or `FALSE`, indicating whether the or not `data_basic` and
`data_augmented` contain the same observations and that `data_augmented`
contains all the columns in `data_basic`.

## Details

For efficiency, the inspection takes place in this order:

1.  `FALSE` is returned early if the datasets do not have the same
    number of rows.

2.  `FALSE` is returned early if the first dataset has a column not in
    the second dataset.

3.  The columns in common are sorted and compared using
    [`all.equal()`](https://rdrr.io/r/base/all.equal.html) with
    `check.attributes = FALSE`.

## See also

[`h_check_fits_all_data_same()`](https://openpharma.github.io/mmrm/reference/h_check_fits_all_data_same.md),
which performs this check on the datasets of adjacent elements of a list
of `mmrm` fits.
