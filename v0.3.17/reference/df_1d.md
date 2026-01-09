# Calculation of Degrees of Freedom for One-Dimensional Contrast

**\[stable\]** Calculates the estimate, adjusted standard error, degrees
of freedom, t statistic and p-value for one-dimensional contrast.

## Usage

``` r
df_1d(object, contrast)
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

## Value

List with `est`, `se`, `df`, `t_stat` and `p_val`.

## Examples

``` r
object <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
contrast <- numeric(length(object$beta_est))
contrast[3] <- 1
df_1d(object, contrast)
#> $est
#> [1] 5.643565
#> 
#> $se
#> [1] 0.6656093
#> 
#> $df
#> [1] 157.1382
#> 
#> $t_stat
#> [1] 8.478795
#> 
#> $p_val
#> [1] 1.564869e-14
#> 
```
