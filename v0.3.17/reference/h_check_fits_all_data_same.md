# Predicate Indicating Whether `mmrm` Fits' Datasets Contain the Same Observations

Checks a `list` of `mmrm` fits to see whether all their datasets contain
the same observations and that they only increase in columns from one
dataset to the next (i.e, columns are nested).

## Usage

``` r
h_check_fits_all_data_same(fits)
```

## Arguments

- fits:

  (`list`)  
  list of `mmrm` fits.

## Value

`TRUE` or `FALSE` indicating whether or not the datasets underlying the
elements of `fits` contain the same observations, and that each
dataset's columns are a subset of the next dataset's columns.

## Details

For efficiency, the inspection takes place in this order:

1.  `FALSE` is returned early if not all datasets have the same number
    of rows.

2.  `FALSE` is returned early if a dataset has a column not in the next
    dataset.

3.  The columns in common among adjacent datasets are sorted and
    compared using
    [`all.equal()`](https://rdrr.io/r/base/all.equal.html) with
    `check.attributes = FALSE`.

This function is more efficient than running
[`h_check_columns_nested()`](https://openpharma.github.io/mmrm/reference/h_check_columns_nested.md)
on all adjacent pairs and supplying the results to
[`all()`](https://rdrr.io/r/base/all.html).

## See also

[`h_check_columns_nested()`](https://openpharma.github.io/mmrm/reference/h_check_columns_nested.md),
which performs this check on two data sets.
