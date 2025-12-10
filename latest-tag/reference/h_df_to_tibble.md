# Coerce a Data Frame to a `tibble`

This is used in
[`h_newdata_add_pred()`](https://openpharma.github.io/mmrm/reference/h_newdata_add_pred.md).

## Usage

``` r
h_df_to_tibble(data)
```

## Arguments

- data:

  (`data.frame`)  
  what to coerce.

## Value

The `data` as a `tibble`, potentially with a `.rownames` column.

## Details

This is only a thin wrapper around
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
except giving a useful error message and it checks for `rownames` and
adds them as a new column `.rownames` if they are not just a numeric
sequence as per the
[`tibble::has_rownames()`](https://tibble.tidyverse.org/reference/rownames.html)
decision.
