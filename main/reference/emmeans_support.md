# Support for `emmeans`

**\[stable\]**

This package includes methods that allow `mmrm` objects to be used with
the `emmeans` package. `emmeans` computes estimated marginal means (also
called least-square means) for the coefficients of the MMRM. We can also
e.g. obtain differences between groups by applying
[`pairs()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
on the object returned by
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

## Examples

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
if (require(emmeans)) {
  emmeans(fit, ~ ARMCD | AVISIT)
  pairs(emmeans(fit, ~ ARMCD | AVISIT), reverse = TRUE)
}
#> Loading required package: emmeans
#> Welcome to emmeans.
#> Caution: You lose important information if you filter this package's results.
#> See '? untidy'
#> AVISIT = VIS1:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     3.77 1.070 146   3.514  0.0006
#> 
#> AVISIT = VIS2:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     3.73 0.859 145   4.346  <.0001
#> 
#> AVISIT = VIS3:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     3.08 0.690 131   4.467  <.0001
#> 
#> AVISIT = VIS4:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     4.40 1.680 133   2.617  0.0099
#> 
#> Results are averaged over the levels of: RACE, SEX 
```
