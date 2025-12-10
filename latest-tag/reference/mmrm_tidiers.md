# Tidying Methods for `mmrm` Objects

**\[stable\]**

These methods tidy the estimates from an `mmrm` object into a summary.

## Usage

``` r
# S3 method for class 'mmrm'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'mmrm'
glance(x, ...)

# S3 method for class 'mmrm'
augment(
  x,
  newdata = NULL,
  interval = c("none", "confidence", "prediction"),
  se_fit = (interval != "none"),
  type.residuals = c("response", "pearson", "normalized"),
  ...
)
```

## Arguments

- x:

  (`mmrm`)  
  fitted model.

- conf.int:

  (`flag`)  
  if `TRUE` columns for the lower (`conf.low`) and upper bounds
  (`conf.high`) of coefficient estimates are included.

- conf.level:

  (`number`)  
  defines the range of the optional confidence internal.

- ...:

  only used by
  [`augment()`](https://generics.r-lib.org/reference/augment.html) to
  pass arguments to the
  [`predict.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md)
  method.

- newdata:

  (`data.frame` or `NULL`)  
  optional new data frame.

- interval:

  (`string`)  
  type of interval calculation.

- se_fit:

  (`flag`)  
  whether to return standard errors of fit.

- type.residuals:

  (`string`)  
  passed on to
  [`residuals.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md).

## Functions

- `tidy(mmrm)`: derives tidy `tibble` from an `mmrm` object.

- `glance(mmrm)`: derives `glance` `tibble` from an `mmrm` object.

- `augment(mmrm)`: derives `augment` `tibble` from an `mmrm` object.

## See also

[`mmrm_methods`](https://openpharma.github.io/mmrm/reference/mmrm_methods.md),
[`mmrm_tmb_methods`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md)
for additional methods.

## Examples

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
# Applying tidy method to return summary table of covariate estimates.
fit |> tidy()
#> # A tibble: 11 × 6
#>    term                          estimate std.error    df statistic  p.value
#>    <chr>                            <dbl>     <dbl> <dbl>     <dbl>    <dbl>
#>  1 (Intercept)                    30.8        0.887  219.   34.7    5.96e-91
#>  2 RACEBlack or African American   1.53       0.624  169.    2.45   1.53e- 2
#>  3 RACEWhite                       5.64       0.666  157.    8.48   1.56e-14
#>  4 SEXFemale                       0.326      0.532  166.    0.613  5.41e- 1
#>  5 ARMCDTRT                        3.77       1.07   146.    3.51   5.89e- 4
#>  6 AVISITVIS2                      4.84       0.802  144.    6.04   1.27e- 8
#>  7 AVISITVIS3                     10.3        0.823  156.   12.6    1.86e-25
#>  8 AVISITVIS4                     15.1        1.31   138.   11.5    8.11e-22
#>  9 ARMCDTRT:AVISITVIS2            -0.0419     1.13   139.   -0.0371 9.70e- 1
#> 10 ARMCDTRT:AVISITVIS3            -0.694      1.19   158.   -0.584  5.60e- 1
#> 11 ARMCDTRT:AVISITVIS4             0.624      1.85   130.    0.337  7.36e- 1
fit |> tidy(conf.int = TRUE, conf.level = 0.9)
#> # A tibble: 11 × 8
#>    term           estimate std.error    df statistic  p.value conf.low conf.high
#>    <chr>             <dbl>     <dbl> <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#>  1 (Intercept)     30.8        0.887  219.   34.7    5.96e-91   29.3       32.2 
#>  2 ARMCDTRT         3.77       1.07   146.    3.51   5.89e- 4    2.00       5.55
#>  3 ARMCDTRT:AVIS…  -0.0419     1.13   139.   -0.0371 9.70e- 1   -1.91       1.83
#>  4 ARMCDTRT:AVIS…  -0.694      1.19   158.   -0.584  5.60e- 1   -2.66       1.27
#>  5 ARMCDTRT:AVIS…   0.624      1.85   130.    0.337  7.36e- 1   -2.44       3.69
#>  6 AVISITVIS2       4.84       0.802  144.    6.04   1.27e- 8    3.51       6.17
#>  7 AVISITVIS3      10.3        0.823  156.   12.6    1.86e-25    8.98      11.7 
#>  8 AVISITVIS4      15.1        1.31   138.   11.5    8.11e-22   12.9       17.2 
#>  9 RACEBlack or …   1.53       0.624  169.    2.45   1.53e- 2    0.498      2.56
#> 10 RACEWhite        5.64       0.666  157.    8.48   1.56e-14    4.54       6.74
#> 11 SEXFemale        0.326      0.532  166.    0.613  5.41e- 1   -0.554      1.21
# Applying glance method to return summary table of goodness of fit statistics.
fit |> glance()
#> # A tibble: 1 × 4
#>     AIC   BIC logLik    deviance
#>   <dbl> <dbl> <logLik>     <dbl>
#> 1 3406. 3439. -1693.225    3386.
# Applying augment method to return merged `tibble` of model data, fitted and residuals.
fit |> augment()
#> # A tibble: 537 × 9
#>    .rownames  FEV1 RACE               SEX   ARMCD AVISIT USUBJID .fitted  .resid
#>        <dbl> <dbl> <fct>              <fct> <fct> <fct>  <fct>     <dbl>   <dbl>
#>  1         2  40.0 Black or African … Fema… TRT   VIS2   PT1        41.2  -1.23 
#>  2         4  20.5 Black or African … Fema… TRT   VIS4   PT1        52.1 -31.6  
#>  3         6  31.5 Asian              Male  PBO   VIS2   PT2        35.6  -4.16 
#>  4         7  36.9 Asian              Male  PBO   VIS3   PT2        41.1  -4.24 
#>  5         8  48.8 Asian              Male  PBO   VIS4   PT2        45.8   2.98 
#>  6        10  36.0 Black or African … Fema… PBO   VIS2   PT3        37.5  -1.49 
#>  7        12  37.2 Black or African … Fema… PBO   VIS4   PT3        47.7 -10.5  
#>  8        13  33.9 Asian              Fema… TRT   VIS1   PT4        34.9  -0.985
#>  9        14  33.7 Asian              Fema… TRT   VIS2   PT4        39.7  -5.93 
#> 10        16  54.5 Asian              Fema… TRT   VIS4   PT4        50.6   3.89 
#> # ℹ 527 more rows
fit |> augment(interval = "confidence")
#> # A tibble: 537 × 12
#>    .rownames  FEV1 RACE         SEX   ARMCD AVISIT USUBJID .fitted .lower .upper
#>        <dbl> <dbl> <fct>        <fct> <fct> <fct>  <fct>     <dbl>  <dbl>  <dbl>
#>  1         2  40.0 Black or Af… Fema… TRT   VIS2   PT1        41.2   41.2   41.2
#>  2         4  20.5 Black or Af… Fema… TRT   VIS4   PT1        52.1   52.0   52.2
#>  3         6  31.5 Asian        Male  PBO   VIS2   PT2        35.6   35.6   35.7
#>  4         7  36.9 Asian        Male  PBO   VIS3   PT2        41.1   41.1   41.1
#>  5         8  48.8 Asian        Male  PBO   VIS4   PT2        45.8   45.7   45.9
#>  6        10  36.0 Black or Af… Fema… PBO   VIS2   PT3        37.5   37.4   37.5
#>  7        12  37.2 Black or Af… Fema… PBO   VIS4   PT3        47.7   47.6   47.8
#>  8        13  33.9 Asian        Fema… TRT   VIS1   PT4        34.9   34.8   34.9
#>  9        14  33.7 Asian        Fema… TRT   VIS2   PT4        39.7   39.6   39.7
#> 10        16  54.5 Asian        Fema… TRT   VIS4   PT4        50.6   50.5   50.7
#> # ℹ 527 more rows
#> # ℹ 2 more variables: .se.fit <dbl>, .resid <dbl>
fit |> augment(type.residuals = "pearson")
#> # A tibble: 537 × 9
#>    .rownames  FEV1 RACE                SEX   ARMCD AVISIT USUBJID .fitted .resid
#>        <dbl> <dbl> <fct>               <fct> <fct> <fct>  <fct>     <dbl>  <dbl>
#>  1         2  40.0 Black or African A… Fema… TRT   VIS2   PT1        41.2 -0.240
#>  2         4  20.5 Black or African A… Fema… TRT   VIS4   PT1        52.1 -3.23 
#>  3         6  31.5 Asian               Male  PBO   VIS2   PT2        35.6 -0.807
#>  4         7  36.9 Asian               Male  PBO   VIS3   PT2        41.1 -1.10 
#>  5         8  48.8 Asian               Male  PBO   VIS4   PT2        45.8  0.305
#>  6        10  36.0 Black or African A… Fema… PBO   VIS2   PT3        37.5 -0.288
#>  7        12  37.2 Black or African A… Fema… PBO   VIS4   PT3        47.7 -1.08 
#>  8        13  33.9 Asian               Fema… TRT   VIS1   PT4        34.9 -0.155
#>  9        14  33.7 Asian               Fema… TRT   VIS2   PT4        39.7 -1.15 
#> 10        16  54.5 Asian               Fema… TRT   VIS4   PT4        50.6  0.398
#> # ℹ 527 more rows
```
