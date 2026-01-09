# Calculate the Significance of Each Term in an `mmrm` Fit.

Runs [`df_md()`](https://openpharma.github.io/mmrm/reference/df_md.md)
once for each term in an `mmrm` object and returns the results in a data
frame.

## Usage

``` r
h_anova_single_mmrm_model(object)
```

## Arguments

- object:

  (`mmrm`)  
  an `mmrm` object.

## Value

A data frame with a row for each term in `object`, including an
intercept if present. The
[row.names](https://rdrr.io/r/base/row.names.html) of the data frame
identify the terms. The data frame will contain the following four
columns:

- `num_df`: the numerator degrees of freedom.

- `denom_df`: the denominator degrees of freedom.

- `f_stat`: the test statistic on the F-distribution.

- `p_va`: the associated p-value.

## Details

When only one model fit is passed to
[`anova.mmrm()`](https://openpharma.github.io/mmrm/reference/stats_anova.md),
the `object` argument of
[`anova.mmrm()`](https://openpharma.github.io/mmrm/reference/stats_anova.md)
is passed directly to this function.
