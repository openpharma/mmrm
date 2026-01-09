# Calculation of Degrees of Freedom for Multi-Dimensional Contrast

**\[stable\]** Calculates the estimate, standard error, degrees of
freedom, t statistic and p-value for m-dimensional contrast, depending
on the method used in
[`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md).

## Usage

``` r
df_md(object, contrast)
```

## Arguments

- object:

  (`mmrm`)  
  the MMRM fit.

- contrast:

  (`matrix`)  
  numeric contrast matrix, if given a `numeric` then this is coerced to
  a row vector. Note that this should not include elements for singular
  coefficient estimates, i.e. only refer to the actually estimated
  coefficients.

## Value

List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).

## Examples

``` r
object <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
contrast <- matrix(data = 0, nrow = 2, ncol = length(object$beta_est))
contrast[1, 2] <- contrast[2, 3] <- 1
df_md(object, contrast)
#> $num_df
#> [1] 2
#> 
#> $denom_df
#> [1] 165.5553
#> 
#> $f_stat
#> [1] 36.91143
#> 
#> $p_val
#> [1] 5.544575e-14
#> 
```
