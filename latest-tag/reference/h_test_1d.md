# Creating T-Statistic Test Results For One-Dimensional Contrast

Creates a list of results for one-dimensional contrasts using a t-test
statistic and the given degrees of freedom.

## Usage

``` r
h_test_1d(object, contrast, df)
```

## Arguments

- object:

  (`mmrm`)  
  the MMRM fit.

- contrast:

  (`numeric`)  
  contrast vector. Note that this should not include elements for
  singular coefficient estimates, i.e. only refer to the actually
  estimated coefficients.

- df:

  (`number`)  
  degrees of freedom for the one-dimensional contrast.

## Value

List with `est`, `se`, `df`, `t_stat` and `p_val` (2-sided p-value).
