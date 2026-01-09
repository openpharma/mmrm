# Calculating Denominator Degrees of Freedom for the Multi-Dimensional Case

Calculates the degrees of freedom for multi-dimensional contrast.

## Usage

``` r
h_md_denom_df(t_stat_df)
```

## Arguments

- t_stat_df:

  (`numeric`)  
  `n` t-statistic derived degrees of freedom.

## Value

Usually the calculation is returning `2 * E / (E - n)` where `E` is the
sum of `t / (t - 2)` over all `t_stat_df` values `t`.

## Note

If the input values are two similar to each other then just the average
of them is returned. If any of the inputs is not larger than 2 then 2 is
returned.
