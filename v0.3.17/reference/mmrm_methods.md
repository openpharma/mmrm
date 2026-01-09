# Methods for `mmrm` Objects

**\[stable\]**

## Usage

``` r
# S3 method for class 'mmrm'
summary(object, ...)

# S3 method for class 'summary.mmrm'
print(
  x,
  digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"),
  ...
)

# S3 method for class 'mmrm'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  (`mmrm`)  
  the fitted MMRM including Jacobian and call etc.

- ...:

  not used.

## Value

Depends on the method, see Details and Functions.

## Details

While printing the summary of (`mmrm`)  
object, the following will be displayed:

1.  Formula. The formula used in the model.

2.  Data. The data used for analysis, including number of subjects,
    number of valid observations.

3.  Covariance. The covariance structure and number of variance
    parameters.

4.  Method. Restricted maximum likelihood(REML) or maximum
    likelihood(ML).

5.  Model selection criteria. AIC, BIC, log likelihood and deviance.

6.  Coefficients. Coefficients of the covariates.

7.  Covariance estimate. The covariance estimate(for each group).

    1.  If the covariance structure is non-spatial, the covariance
        matrix of all categorical time points available in data will be
        displayed.

    2.  If the covariance structure is spatial, the covariance matrix of
        two time points with unit distance will be displayed.

`confint` is used to obtain the confidence intervals for the
coefficients. Please note that this is different from the confidence
interval of difference of least square means from `emmeans`.

## Functions

- `summary(mmrm)`: summarizes the MMRM fit results.

- `print(summary.mmrm)`: prints the MMRM fit summary.

- `confint(mmrm)`: obtain the confidence intervals for the coefficients.

## See also

[`mmrm_tmb_methods`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md),
[`mmrm_tidiers`](https://openpharma.github.io/mmrm/reference/mmrm_tidiers.md)
for additional methods.

## Examples

``` r
formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
object <- mmrm(formula, fev_data)
# Summary:
summary(object)
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Method:      Satterthwaite
#> Vcov Method: Asymptotic
#> Inference:   REML
#> 
#> Model selection criteria:
#>      AIC      BIC   logLik deviance 
#>   3406.4   3439.3  -1693.2   3386.4 
#> 
#> Coefficients: 
#>                                Estimate Std. Error        df t value Pr(>|t|)
#> (Intercept)                    30.77748    0.88656 218.80000  34.715  < 2e-16
#> RACEBlack or African American   1.53050    0.62448 168.67000   2.451 0.015272
#> RACEWhite                       5.64357    0.66561 157.14000   8.479 1.56e-14
#> SEXFemale                       0.32606    0.53195 166.13000   0.613 0.540744
#> ARMCDTRT                        3.77423    1.07415 145.55000   3.514 0.000589
#> AVISITVIS2                      4.83959    0.80172 143.88000   6.037 1.27e-08
#> AVISITVIS3                     10.34211    0.82269 155.56000  12.571  < 2e-16
#> AVISITVIS4                     15.05390    1.31281 138.47000  11.467  < 2e-16
#> ARMCDTRT:AVISITVIS2            -0.04193    1.12932 138.56000  -0.037 0.970439
#> ARMCDTRT:AVISITVIS3            -0.69369    1.18765 158.17000  -0.584 0.559996
#> ARMCDTRT:AVISITVIS4             0.62423    1.85085 129.72000   0.337 0.736463
#>                                  
#> (Intercept)                   ***
#> RACEBlack or African American *  
#> RACEWhite                     ***
#> SEXFemale                        
#> ARMCDTRT                      ***
#> AVISITVIS2                    ***
#> AVISITVIS3                    ***
#> AVISITVIS4                    ***
#> ARMCDTRT:AVISITVIS2              
#> ARMCDTRT:AVISITVIS3              
#> ARMCDTRT:AVISITVIS4              
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Covariance estimate:
#>         VIS1    VIS2    VIS3    VIS4
#> VIS1 40.5537 14.3960  4.9747 13.3867
#> VIS2 14.3960 26.5715  2.7855  7.4745
#> VIS3  4.9747  2.7855 14.8979  0.9082
#> VIS4 13.3867  7.4745  0.9082 95.5568
#> 
# Confidence Interval:
confint(object)
#>                                    2.5 %    97.5 %
#> (Intercept)                   29.0301757 32.524775
#> RACEBlack or African American  0.2977051  2.763294
#> RACEWhite                      4.3288700  6.958261
#> SEXFemale                     -0.7241923  1.376316
#> ARMCDTRT                       1.6512897  5.897170
#> AVISITVIS2                     3.2549236  6.424253
#> AVISITVIS3                     8.7170213 11.967204
#> AVISITVIS4                    12.4581459 17.649651
#> ARMCDTRT:AVISITVIS2           -2.2748504  2.190998
#> ARMCDTRT:AVISITVIS3           -3.0393757  1.652005
#> ARMCDTRT:AVISITVIS4           -3.0375371  4.285991
```
