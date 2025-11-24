# Conduct type II/III hypothesis testing on the MMRM fit results.

Conduct type II/III hypothesis testing on the MMRM fit results.

## Usage

``` r
Anova.mmrm(
  mod,
  type = c("II", "III", "2", "3"),
  tol = sqrt(.Machine$double.eps),
  test.statistic = c("F", "Chisq"),
  ...
)
```

## Arguments

- mod:

  (`mmrm`)  
  the fitted MMRM.

- type:

  (`string`)  
  either `"II"`, `"III"`, `"2"`, or `"3"`, indicating the type of test
  to perform.

- tol:

  (`numeric`) threshold below which values are treated as 0.

- test.statistic:

  (`string`)  
  either `"F` or `"Chisq"`, indicating the kind of test to perform.

- ...:

  arguments passed from other methods.

## Details

`Anova()` will return an `anova` object with one row per variable.

If `test.statistic = "F"`, columns will be `Df`(numerator degrees of
freedom), `Res.Df` (denominator degrees of freedom), `F`, and `Pr(>F)`.

If `test.statistic = "Chisq"`, columns will be `Chisq` (the Chi-squared
test statistic), `Df` (degrees of freedom), and `Pr(>Chisq)` (p-value).
