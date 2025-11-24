# Between-Within

For determining the degrees of freedom (DF) required for the testing of
fixed effects, one option is to use the “between-within” method,
originally proposed by Schluchter and Elashoff (1990) as a small-sample
adjustment.

## General definition

Using this method, the DF are determined by the grouping level at which
the term is estimated. Generally, assuming $`G`$ levels of grouping:

$`DF_g=N_g-(N_{g-1}+p_g), g=1, ..., G+1`$

where $`N_g`$ is the number of groups at the $`g`$-th grouping level and
$`p_g`$ is the number of parameters estimated at that level.

$`N_0=1`$ if the model includes an intercept term and $`N_0=0`$
otherwise. Note however that the DF for the intercept term itself (when
it is included) are calculated at the $`G+1`$ level, i.e. for the
intercept we use $`DF_{G+1}`$ degrees of freedom.

We note that general contrasts $`C\beta`$ have not been considered in
the literature so far. Here we therefore use a pragmatic approach and
define that for a general contrast matrix $`C`$ we take the minimum DF
across the involved coefficients as the DF.

## MMRM special case

In our case of an MMRM (with only fixed effect terms), there is only a
single grouping level (subject), so $`G=1`$. This means there are 3
potential “levels” of parameters (Gałecki and Burzykowski (2013)):

- Level 0: The intercept term, assuming the model has been fitted with
  one.
  - We use $`DF_2`$ degrees of freedom as defined below.
- Level 1: Effects that change between subjects, but not across
  observations within subjects.
  - These are the “between parameters”.
  - The corresponding degrees of freedom are
    $`DF_1 = N_1 - (N_0 + p_1)`$.
  - In words this can be read as:  
    “Between” DF = “number of subjects” - (“1 if intercept otherwise
    0” + “number of between parameters”).
- Level 2: Effects that change within subjects.
  - These are the “within parameters”.
  - The corresponding degrees of freedom are
    $`DF_2 = N_2 - (N_1 + p_2)`$.
  - In words this can be read as:  
    “Within” DF = “number of observations” - (“number of subjects” +
    “number of within parameters”).

## Example

Let’s look at a concrete example and what the “between-within” degrees
of freedom method gives as results:

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  control = mmrm_control(method = "Between-Within")
)
summary(fit)
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Method:      Between-Within
#> Vcov Method: Asymptotic
#> Inference:   REML
#> 
#> Model selection criteria:
#>      AIC      BIC   logLik deviance 
#>   3406.4   3439.3  -1693.2   3386.4 
#> 
#> Coefficients: 
#>                                Estimate Std. Error        df t value Pr(>|t|)
#> (Intercept)                    30.77748    0.88656 334.00000  34.715  < 2e-16
#> RACEBlack or African American   1.53050    0.62448 192.00000   2.451 0.015147
#> RACEWhite                       5.64357    0.66561 192.00000   8.479 5.98e-15
#> SEXFemale                       0.32606    0.53195 192.00000   0.613 0.540631
#> ARMCDTRT                        3.77423    1.07415 192.00000   3.514 0.000551
#> AVISITVIS2                      4.83959    0.80172 334.00000   6.037 4.19e-09
#> AVISITVIS3                     10.34211    0.82269 334.00000  12.571  < 2e-16
#> AVISITVIS4                     15.05390    1.31281 334.00000  11.467  < 2e-16
#> ARMCDTRT:AVISITVIS2            -0.04193    1.12932 334.00000  -0.037 0.970407
#> ARMCDTRT:AVISITVIS3            -0.69369    1.18765 334.00000  -0.584 0.559558
#> ARMCDTRT:AVISITVIS4             0.62423    1.85085 334.00000   0.337 0.736129
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
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Covariance estimate:
#>         VIS1    VIS2    VIS3    VIS4
#> VIS1 40.5537 14.3960  4.9747 13.3867
#> VIS2 14.3960 26.5715  2.7855  7.4745
#> VIS3  4.9747  2.7855 14.8979  0.9082
#> VIS4 13.3867  7.4745  0.9082 95.5568
```

Let’s try to calculate the degrees of freedom manually now.

In `fev_data` there are 197 subjects with at least one non-missing
`FEV1` observation, and 537 non-missing observations in total. Therefore
we obtain the following numbers of groups $`N_g`$ at the levels
$`g=1,2`$:

- $`N_1 = 197`$
- $`N_2 = 537`$

And we note that $`N_0 = 1`$ because we use an intercept term.

Now let’s look at the design matrix:

``` r
head(model.matrix(fit), 1)
#>   (Intercept) RACEBlack or African American RACEWhite SEXFemale ARMCDTRT
#> 2           1                             1         0         1        1
#>   AVISITVIS2 AVISITVIS3 AVISITVIS4 ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
#> 2          1          0          0                   1                   0
#>   ARMCDTRT:AVISITVIS4
#> 2                   0
```

Leaving the intercept term aside, we therefore have the following number
of parameters for the corresponding effects:

- `RACE`: 2
- `SEX`: 1
- `ARMCD`: 1
- `AVISIT`: 3
- `ARMCD:AVISIT`: 3

In the model above, `RACE`, `SEX` and `ARMCD` are between-subjects
effects and belong to level 1; they do not vary within subject across
the repeated observations. On the other hand, `AVISIT` is a
within-subject effect; it represents study visit, so naturally its value
changes over repeated observations for each subject. Similarly, the
interaction of `ARMCD` and `AVISIT` also belongs to level 2.

Therefore we obtain the following numbers of parameters $`p_g`$ at the
levels $`g=1,2`$:

- $`p_1 = 2 + 1 + 1 = 4`$
- $`p_2 = 3 + 3 = 6`$

And we obtain therefore the degrees of freedom $`DF_g`$ at the levels
$`g=1,2`$:

- $`DF_1 = N_1 - (N_0 + p_1) = 197 - (1 + 4) = 192`$
- $`DF_2 = N_2 - (N_1 + p_2) = 537 - (197 + 6) = 334`$

So we can finally see that those degrees of freedom are exactly as
displayed in the summary table above.

## Differences compared to SAS

The implementation described above is not identical to that of SAS.
Differences include:

- In SAS, when using an unstructured covariance matrix, all effects are
  assigned the between-subjects degrees of freedom.
- In SAS, the within-subjects degrees of freedom are affected by the
  number of subjects in which the effect takes different values.
- In SAS, if there are multiple within-subject effects containing
  classification variables, the within-subject degrees of freedom are
  partitioned into components corresponding to the subject-by-effect
  interactions.
- In SAS, the final effect you list in the `CONTRAST`/`ESTIMATE`
  statement is used to define the DF for general contrasts.

Code contributions for adding the SAS version of between-within degrees
of freedom to the `mmrm` package are welcome!

## References

Gałecki A, Burzykowski T (2013). “Linear Mixed-Effects Model.” In
*Linear mixed-effects models using r* 245–273. Springer.

Schluchter MD, Elashoff JT (1990). “Small-Sample Adjustments to Tests
with Unbalanced Repeated Measures Assuming Several Covariance
Structures.” *Journal of Statistical Computation and Simulation*,
**37**(1-2), 69–87.
