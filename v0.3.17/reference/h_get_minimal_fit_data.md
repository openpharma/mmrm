# Obtain the Minimal Dataset Needed for an `mmrm` Fit

Grab the dataset underlying an `mmrm` fit and select only the used
columns.

## Usage

``` r
h_get_minimal_fit_data(fit)
```

## Arguments

- fit:

  (`mmrm`)  
  a fitted `mmrm` model.

## Value

A data frame: a subset of the columns the dataset underlying `fit`
(i.e., `fit$data`):

- The response column.

- The column denoting the visit index.

- The column denoting the subject.

- The column denoting the subject's grouping (e.g., study arm).

- All other predictors not already specified.

  Columns that were not used are excluded.

## Details

Grabs the response variable along with the predictors named in
`fit$formula_parts`.
