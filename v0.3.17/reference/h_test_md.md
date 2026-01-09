# Creating F- or Chi-squared-statistic Test Results For Multi-Dimensional Contrast

Creates a list of results for multi-dimensional contrasts using an
F-test statistic and the given degrees of freedom or a Chi-squared test
statistic.

## Usage

``` r
h_test_md(object, contrast, df, f_stat_factor = 1, test = c("F", "Chisq"))
```

## Arguments

- object:

  (`mmrm`)  
  the MMRM fit.

- contrast:

  (`matrix`)  
  numeric contrast matrix.

- df:

  (`number`)  
  denominator degrees of freedom for the multi-dimensional contrast for
  an F-test. Ignored if `test = "Chisq"`.

- f_stat_factor:

  (`number`)  
  optional scaling factor on top of the standard F-statistic. Ignored if
  `test = "Chisq"`.

- test:

  (`string`)  
  either `"F"` or `"Chisq"`, specifying the kind of test to perform.

## Value

If `test = "F"`, a list with `num_df`, `denom_df`, `f_stat` and `p_val`.
If `test = "Chisq"`, a list with `df`, `chisq_stat`, and `p_val`. In
both cases, p-values are two sided.
