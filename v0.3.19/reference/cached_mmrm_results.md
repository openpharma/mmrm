# Cache Data for `mmrm` Model Comparison

**\[stable\]**

## Usage

``` r
cached_mmrm_results
```

## Format

A `list` with following elements:

- `conv_time_fev`: Convergence time on FEV data.

- `conv_time_bcva`: Convergence time on BCVA data.

- `rel_diff_ests_tbl_fev`: Relative difference in estimates on FEV data.

- `rel_diff_ests_tbl_bcva`: Relative difference in estimates on BCVA
  data.

- `conv_rate`: Convergence rate on data with different missing levels.

- `df_missingness`: Summary of missingness on simulated data.

## Source

This is created based on simulations on FEV data and BCVA data.

## Note

The cached data for comparison is used for the vignettes generation.
Please make sure that this data is refreshed before each package release
by running the script `data-raw/mmrm_review.R`. Please make sure to
install the `mmrm` package instead of using
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
before running the script to achieve accurate timings.
