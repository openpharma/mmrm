# Methods for `mmrm_tmb` Objects

**\[stable\]**

## Usage

``` r
# S3 method for class 'mmrm_tmb'
coef(object, complete = TRUE, ...)

# S3 method for class 'mmrm_tmb'
fitted(object, ...)

# S3 method for class 'mmrm_tmb'
predict(
  object,
  newdata,
  se.fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  nsim = 1000L,
  conditional = FALSE,
  ...
)

# S3 method for class 'mmrm_tmb'
model.frame(
  formula,
  data,
  include = c("subject_var", "visit_var", "group_var", "response_var"),
  exclude = NULL,
  full,
  na.action = "na.omit",
  ...
)

# S3 method for class 'mmrm_tmb'
model.matrix(
  object,
  data,
  use_response = TRUE,
  contrasts.arg = attr(object$tmb_data$x_matrix, "contrasts"),
  ...
)

# S3 method for class 'mmrm_tmb'
terms(x, include = "response_var", ...)

# S3 method for class 'mmrm_tmb'
logLik(object, ...)

# S3 method for class 'mmrm_tmb'
formula(x, ...)

# S3 method for class 'mmrm_tmb'
vcov(object, complete = TRUE, ...)

# S3 method for class 'mmrm_tmb'
VarCorr(x, sigma = NA, ...)

# S3 method for class 'mmrm_tmb'
deviance(object, ...)

# S3 method for class 'mmrm_tmb'
AIC(object, corrected = FALSE, ..., k = 2)

# S3 method for class 'mmrm_tmb'
BIC(object, ...)

# S3 method for class 'mmrm_tmb'
print(x, ...)

# S3 method for class 'mmrm_tmb'
residuals(object, type = c("response", "pearson", "normalized"), ...)

# S3 method for class 'mmrm_tmb'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  newdata,
  ...,
  method = c("conditional", "marginal")
)
```

## Arguments

- object:

  (`mmrm_tmb`)  
  the fitted MMRM object.

- complete:

  (`flag`)  
  whether to include potential non-estimable coefficients.

- ...:

  mostly not used; Exception is
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) passing
  `...` to the default method.

- newdata:

  (`data.frame`)  
  optional new data, otherwise data from `object` is used.

- se.fit:

  (`flag`)  
  indicator if standard errors are required.

- interval:

  (`string`)  
  type of interval calculation. Can be abbreviated.

- level:

  (`number`)  
  tolerance/confidence level.

- nsim:

  (`count`)  
  number of simulations to use.

- conditional:

  (`flag`)  
  indicator if the prediction is conditional on the observation or not.

- formula:

  (`mmrm_tmb`)  
  same as `object`.

- data:

  (`data.frame`)  
  object in which to construct the frame.

- include:

  (`character`)  
  names of variable types to include in the model frame creation. Must
  be `NULL` or one or more of
  `c("subject_var", "visit_var", "group_var", "response_var")`.

- exclude:

  (`character`)  
  names of variable types to exclude after the model frame creation.
  Same choices as for `include`.

- full:

  (`flag`)  
  indicator whether to return full model frame (deprecated).

- na.action:

  (`string`)  
  na action.

- use_response:

  (`flag`)  
  whether to use the response for complete rows.

- contrasts.arg:

  (`list`)  
  contrasts to be used.

- x:

  (`mmrm_tmb`)  
  same as `object`.

- sigma:

  cannot be used (this parameter does not exist in MMRM).

- corrected:

  (`flag`)  
  whether corrected AIC should be calculated.

- k:

  (`number`)  
  the penalty per parameter to be used; default `k = 2` is the classical
  AIC.

- type:

  (`string`)  
  unscaled (`response`), `pearson` or `normalized`. Default is
  `response`, and this is the only type available for use with models
  with a spatial covariance structure.

- seed:

  unused argument from
  [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html).

- method:

  (`string`)  
  simulation method to use. If "conditional", simulated values are
  sampled given the estimated covariance matrix of `object`. If
  "marginal", the variance of the estimated covariance matrix is taken
  into account.

## Value

Depends on the method, see Functions.

## Details

`include` argument controls the variables the model frame will include
upon creation. Possible options are "response_var", "subject_var",
"visit_var" and "group_var", representing the response variable, subject
variable, visit variable or group variable. By default all four variable
types are included, which means that the original model frame will be
used. If only a subset of variable types is requested, the model frame
will be constructed freshly, and if there are more complete rows
(without `NA`s) in the required variables than in the original model
frame, those rows will be included as well.

The `exclude` argument can be used to exclude specific variable types
after the model frame creation. By default no variable types are
excluded. The `exclude` argument is useful when the original model frame
should be used as basis (with same incomplete rows omitted), but some
variable types should be removed afterwards.

`character` values in new data will always be factorized according to
the data in the fit to avoid mismatched in levels or issues in
`model.matrix`.

## Functions

- `coef(mmrm_tmb)`: obtains the estimated coefficients.

- `fitted(mmrm_tmb)`: obtains the fitted values.

- `predict(mmrm_tmb)`: predict conditional means for new data;
  optionally with standard errors and confidence or prediction
  intervals. Returns a vector of predictions if `se.fit == FALSE` and
  `interval == "none"`; otherwise it returns a data.frame with multiple
  columns and one row per input data row.

- `model.frame(mmrm_tmb)`: obtains the model frame.

- `model.matrix(mmrm_tmb)`: obtains the model matrix.

- `terms(mmrm_tmb)`: obtains the terms object.

- `logLik(mmrm_tmb)`: obtains the attained log likelihood value.
  Includes as attributes the number of variance parameters `n_param`,
  number of estimated coefficients `n_coef`, degrees of freedom `df`,
  and number of subjects `nobs`. The `nobs` attribute is so named so
  that if this function's results are passed to
  [`stats::BIC()`](https://rdrr.io/r/stats/AIC.html), the BIC value will
  be calculated correctly. Resulting value is of class
  [`logLik`](https://rdrr.io/r/stats/logLik.html).

- `formula(mmrm_tmb)`: obtains the used formula.

- `vcov(mmrm_tmb)`: obtains the variance-covariance matrix estimate for
  the coefficients.

- `VarCorr(mmrm_tmb)`: obtains the variance-covariance matrix estimate
  for the residuals.

- `deviance(mmrm_tmb)`: obtains the deviance, which is defined here as
  twice the negative log likelihood, which can either be integrated over
  the coefficients for REML fits or the usual one for ML fits.

- `AIC(mmrm_tmb)`: obtains the Akaike Information Criterion, where the
  degrees of freedom are the number of variance parameters (`n_theta`).
  If `corrected`, then this is multiplied with `m / (m - n_theta - 1)`
  where `m` is the number of observations minus the number of
  coefficients, or `n_theta + 2` if it is smaller than that (Hurvich and
  Tsai 1989; Burnham and Anderson 1998) .

- `BIC(mmrm_tmb)`: obtains the Bayesian Information Criterion, which is
  using the natural logarithm of the number of subjects for the penalty
  parameter `k`.

- `print(mmrm_tmb)`: prints the object.

- `residuals(mmrm_tmb)`: to obtain residuals - either unscaled
  ('response'), 'pearson' or 'normalized'.

- `simulate(mmrm_tmb)`: simulate responses from a fitted model according
  to the simulation `method`, returning a `data.frame` of dimension
  `[n, m]` where n is the number of rows in `newdata`, and m is the
  number `nsim` of simulated responses.

## Note

If the response in the formula is a complicated expression, e.g. with
multiple variables, and not all variables are provided in `newdata`, a
warning is issued. In this case it is recommended to provide all
variables in the `newdata` data set, and manually set the response
variables to `NA` as needed for obtaining unconditional predictions e.g.

## References

- Hurvich CM, Tsai C (1989). “Regression and time series model selection
  in small samples.” *Biometrika*, **76**(2), 297–307.
  [doi:10.2307/2336663](https://doi.org/10.2307/2336663) .

- Burnham KP, Anderson DR (1998). “Practical use of the
  information-theoretic approach.” In *Model selection and inference*,
  75–117. Springer.
  [doi:10.1007/978-1-4757-2917-7_3](https://doi.org/10.1007/978-1-4757-2917-7_3)
  .

&nbsp;

- Gałecki A, Burzykowski T (2013). “Linear mixed-effects model.” In
  *Linear mixed-effects models using R*, 245–273. Springer.

## See also

[`mmrm_methods`](https://openpharma.github.io/mmrm/reference/mmrm_methods.md),
[`mmrm_tidiers`](https://openpharma.github.io/mmrm/reference/mmrm_tidiers.md)
for additional methods.

## Examples

``` r
formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
object <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)))
# Estimated coefficients:
coef(object)
#>                   (Intercept) RACEBlack or African American 
#>                   30.77747548                    1.53049977 
#>                     RACEWhite                     SEXFemale 
#>                    5.64356535                    0.32606192 
#>                      ARMCDTRT                    AVISITVIS2 
#>                    3.77423004                    4.83958845 
#>                    AVISITVIS3                    AVISITVIS4 
#>                   10.34211288                   15.05389826 
#>           ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
#>                   -0.04192625                   -0.69368537 
#>           ARMCDTRT:AVISITVIS4 
#>                    0.62422703 
# Fitted values:
fitted(object)
#>        2        4        6        7        8       10       12       13 
#> 41.20593 52.08639 35.61706 41.11959 45.83137 37.47363 47.68794 34.87777 
#>       14       16       17       19       20       23       25       26 
#> 39.67543 50.55589 32.30798 42.65009 47.36187 42.65009 31.10354 35.94313 
#>       28       29       30       31       32       33       34       36 
#> 46.15744 32.30798 37.14756 42.65009 47.36187 40.19527 44.99293 55.87340 
#>       39       41       42       43       44       45       46       47 
#> 42.97615 34.87777 39.67543 44.52619 50.55589 30.77748 35.61706 41.11959 
#>       51       52       55       59       60       62       64       65 
#> 49.84370 55.87340 42.65009 41.11959 45.83137 35.94313 46.15744 36.74710 
#>       68       69       70       72       73       74       75       76 
#> 51.80100 34.55171 39.34937 50.22983 34.55171 39.34937 44.20013 50.22983 
#>       78       79       82       83       84       85       86       87 
#> 45.31900 50.16976 44.99293 49.84370 55.87340 40.19527 44.99293 49.84370 
#>       88       89       90       91       93       94       95       96 
#> 55.87340 32.63404 37.47363 42.97615 40.52133 45.31900 50.16976 56.19946 
#>       97       98       99      100      101      102      103      104 
#> 40.52133 45.31900 50.16976 56.19946 40.52133 45.31900 50.16976 56.19946 
#>      105      107      108      109      110      111      112      113 
#> 36.40827 46.05669 52.08639 36.08221 40.87987 45.73063 51.76033 36.08221 
#>      114      116      117      118      119      120      121      123 
#> 40.87987 51.76033 34.87777 39.67543 44.52619 50.55589 31.10354 41.44565 
#>      125      128      129      130      132      133      134      135 
#> 36.08221 51.76033 36.08221 40.87987 51.76033 31.10354 35.94313 41.44565 
#>      136      137      138      140      142      144      145      146 
#> 46.15744 36.40827 41.20593 52.08639 39.67543 50.55589 36.74710 41.58669 
#>      147      148      149      151      153      155      156      157 
#> 47.08922 51.80100 31.10354 41.44565 31.10354 41.44565 46.15744 32.30798 
#>      158      159      162      163      164      165      168      169 
#> 37.14756 42.65009 37.14756 42.65009 47.36187 32.30798 47.36187 36.40827 
#>      170      171      172      173      177      178      179      180 
#> 41.20593 46.05669 52.08639 36.08221 31.10354 35.94313 41.44565 46.15744 
#>      181      182      183      185      186      187      190      191 
#> 30.77748 35.61706 41.11959 34.55171 39.34937 44.20013 37.14756 42.65009 
#>      193      194      195      197      198      199      201      202 
#> 36.74710 41.58669 47.08922 34.87777 39.67543 44.52619 36.40827 41.20593 
#>      204      206      208      209      210      217      218      219 
#> 52.08639 41.20593 52.08639 36.42104 41.26063 34.55171 39.34937 44.20013 
#>      221      224      226      227      228      230      231      233 
#> 32.30798 47.36187 40.87987 45.73063 51.76033 45.31900 50.16976 40.52133 
#>      235      236      237      238      239      240      241      242 
#> 50.16976 56.19946 36.40827 41.20593 46.05669 52.08639 34.55171 39.34937 
#>      244      246      250      251      252      253      254      256 
#> 50.22983 35.94313 41.58669 47.08922 51.80100 32.63404 37.47363 47.68794 
#>      257      258      259      260      261      262      263      264 
#> 34.87777 39.67543 44.52619 50.55589 34.55171 39.34937 44.20013 50.22983 
#>      265      266      267      268      269      270      273      274 
#> 40.52133 45.31900 50.16976 56.19946 40.52133 45.31900 32.30798 37.14756 
#>      275      276      277      278      280      281      282      283 
#> 42.65009 47.36187 32.30798 37.14756 47.36187 34.55171 39.34937 44.20013 
#>      284      285      286      287      291      292      293      295 
#> 50.22983 34.87777 39.67543 44.52619 46.76315 51.47494 36.74710 47.08922 
#>      296      298      299      300      301      304      305      306 
#> 51.80100 41.26063 46.76315 51.47494 40.19527 55.87340 34.55171 39.34937 
#>      307      308      310      311      312      316      317      318 
#> 44.20013 50.22983 45.31900 50.16976 56.19946 46.15744 34.55171 39.34937 
#>      319      322      323      324      325      327      328      329 
#> 44.20013 41.26063 46.76315 51.47494 32.63404 42.97615 47.68794 40.19527 
#>      330      331      332      336      339      340      341      342 
#> 44.99293 49.84370 55.87340 46.15744 41.44565 46.15744 36.40827 41.20593 
#>      343      344      345      347      349      351      352      353 
#> 46.05669 52.08639 36.42104 46.76315 32.63404 42.97615 47.68794 32.30798 
#>      354      355      356      357      363      364      365      367 
#> 37.14756 42.65009 47.36187 30.77748 50.16976 56.19946 40.52133 50.16976 
#>      368      370      371      372      373      375      376      378 
#> 56.19946 37.14756 42.65009 47.36187 31.10354 41.44565 46.15744 35.94313 
#>      379      381      382      384      385      386      388      389 
#> 41.44565 32.63404 37.47363 47.68794 40.19527 44.99293 55.87340 30.77748 
#>      390      391      392      394      397      398      399      402 
#> 35.61706 41.11959 45.83137 37.14756 32.63404 37.47363 42.97615 44.99293 
#>      403      405      406      407      408      409      410      411 
#> 49.84370 31.10354 35.94313 41.44565 46.15744 31.10354 35.94313 41.44565 
#>      413      415      416      418      419      421      422      423 
#> 32.63404 42.97615 47.68794 41.20593 46.05669 34.87777 39.67543 44.52619 
#>      424      427      428      429      430      431      432      434 
#> 50.55589 47.08922 51.80100 40.19527 44.99293 49.84370 55.87340 37.47363 
#>      435      436      438      439      444      445      447      449 
#> 42.97615 47.68794 39.34937 44.20013 51.80100 32.63404 42.97615 30.77748 
#>      450      451      453      454      455      456      457      458 
#> 35.61706 41.11959 32.30798 37.14756 42.65009 47.36187 31.10354 35.94313 
#>      459      461      463      464      465      469      470      471 
#> 41.44565 36.08221 45.73063 51.76033 32.30798 31.10354 35.94313 41.44565 
#>      473      474      477      484      487      489      490      491 
#> 40.19527 44.99293 31.10354 51.76033 41.44565 36.74710 41.58669 47.08922 
#>      494      495      496      497      498      499      501      502 
#> 39.67543 44.52619 50.55589 36.42104 41.26063 46.76315 34.55171 39.34937 
#>      504      505      508      509      510      511      512      513 
#> 50.22983 36.40827 52.08639 34.87777 39.67543 44.52619 50.55589 40.19527 
#>      518      519      521      522      523      524      526      527 
#> 41.26063 46.76315 31.10354 35.94313 41.44565 46.15744 35.61706 41.11959 
#>      528      530      531      532      534      535      536      537 
#> 45.83137 41.26063 46.76315 51.47494 37.14756 42.65009 47.36187 32.30798 
#>      538      539      540      541      544      545      546      547 
#> 37.14756 42.65009 47.36187 40.52133 56.19946 34.87777 39.67543 44.52619 
#>      548      549      550      551      555      556      557      558 
#> 50.55589 34.55171 39.34937 44.20013 44.52619 50.55589 32.63404 37.47363 
#>      560      562      564      569      570      572      573      574 
#> 47.68794 44.99293 55.87340 36.40827 41.20593 52.08639 31.10354 35.94313 
#>      575      576      577      578      579      582      583      584 
#> 41.44565 46.15744 32.30798 37.14756 42.65009 39.67543 44.52619 50.55589 
#>      585      586      587      590      591      593      594      595 
#> 31.10354 35.94313 41.44565 40.87987 45.73063 31.10354 35.94313 41.44565 
#>      596      599      600      601      602      604      606      608 
#> 46.15744 46.76315 51.47494 31.10354 35.94313 46.15744 39.67543 50.55589 
#>      609      610      611      612      613      614      616      617 
#> 32.30798 37.14756 42.65009 47.36187 32.63404 37.47363 47.68794 34.55171 
#>      619      620      621      622      623      624      625      628 
#> 44.20013 50.22983 36.42104 41.26063 46.76315 51.47494 36.08221 51.76033 
#>      630      631      632      633      634      638      639      640 
#> 40.87987 45.73063 51.76033 34.87777 39.67543 39.34937 44.20013 50.22983 
#>      642      645      648      650      652      654      655      656 
#> 37.47363 30.77748 45.83137 41.26063 51.47494 37.47363 42.97615 47.68794 
#>      657      661      665      666      668      669      670      671 
#> 40.52133 34.55171 32.30798 37.14756 47.36187 32.63404 37.47363 42.97615 
#>      673      674      678      679      680      682      684      685 
#> 36.74710 41.58669 45.31900 50.16976 56.19946 35.94313 46.15744 34.87777 
#>      686      687      689      690      691      693      694      695 
#> 39.67543 44.52619 40.52133 45.31900 50.16976 36.40827 41.20593 46.05669 
#>      697      698      700      701      702      704      707      709 
#> 32.63404 37.47363 47.68794 36.08221 40.87987 51.76033 42.65009 32.30798 
#>      712      713      715      716      717      718      719      721 
#> 47.36187 40.19527 49.84370 55.87340 30.77748 35.61706 41.11959 31.10354 
#>      723      724      727      728      729      730      734      736 
#> 41.44565 46.15744 46.05669 52.08639 36.40827 41.20593 45.31900 56.19946 
#>      738      739      740      741      742      744      745      749 
#> 37.47363 42.97615 47.68794 36.42104 41.26063 51.47494 32.63404 32.63404 
#>      751      753      754      755      757      758      759      760 
#> 42.97615 34.87777 39.67543 44.52619 36.40827 41.20593 46.05669 52.08639 
#>      761      764      765      766      767      768      770      771 
#> 40.19527 55.87340 40.52133 45.31900 50.16976 56.19946 35.61706 41.11959 
#>      772      773      774      775      776      778      779      781 
#> 45.83137 32.30798 37.14756 42.65009 47.36187 45.31900 50.16976 30.77748 
#>      782      784      788      789      790      792      797      798 
#> 35.61706 45.83137 47.36187 36.08221 40.87987 51.76033 32.30798 37.14756 
#>      800 
#> 47.36187 
predict(object, newdata = fev_data)
#>        1        2        3        4        5        6        7        8 
#> 36.40827 41.20593 46.05669 52.08639 30.77748 35.61706 41.11959 45.83137 
#>        9       10       11       12       13       14       15       16 
#> 32.63404 37.47363 42.97615 47.68794 34.87777 39.67543 44.52619 50.55589 
#>       17       18       19       20       21       22       23       24 
#> 32.30798 37.14756 42.65009 47.36187 32.30798 37.14756 42.65009 47.36187 
#>       25       26       27       28       29       30       31       32 
#> 31.10354 35.94313 41.44565 46.15744 32.30798 37.14756 42.65009 47.36187 
#>       33       34       35       36       37       38       39       40 
#> 40.19527 44.99293 49.84370 55.87340 32.63404 37.47363 42.97615 47.68794 
#>       41       42       43       44       45       46       47       48 
#> 34.87777 39.67543 44.52619 50.55589 30.77748 35.61706 41.11959 45.83137 
#>       49       50       51       52       53       54       55       56 
#> 40.19527 44.99293 49.84370 55.87340 32.30798 37.14756 42.65009 47.36187 
#>       57       58       59       60       61       62       63       64 
#> 30.77748 35.61706 41.11959 45.83137 31.10354 35.94313 41.44565 46.15744 
#>       65       66       67       68       69       70       71       72 
#> 36.74710 41.58669 47.08922 51.80100 34.55171 39.34937 44.20013 50.22983 
#>       73       74       75       76       77       78       79       80 
#> 34.55171 39.34937 44.20013 50.22983 40.52133 45.31900 50.16976 56.19946 
#>       81       82       83       84       85       86       87       88 
#> 40.19527 44.99293 49.84370 55.87340 40.19527 44.99293 49.84370 55.87340 
#>       89       90       91       92       93       94       95       96 
#> 32.63404 37.47363 42.97615 47.68794 40.52133 45.31900 50.16976 56.19946 
#>       97       98       99      100      101      102      103      104 
#> 40.52133 45.31900 50.16976 56.19946 40.52133 45.31900 50.16976 56.19946 
#>      105      106      107      108      109      110      111      112 
#> 36.40827 41.20593 46.05669 52.08639 36.08221 40.87987 45.73063 51.76033 
#>      113      114      115      116      117      118      119      120 
#> 36.08221 40.87987 45.73063 51.76033 34.87777 39.67543 44.52619 50.55589 
#>      121      122      123      124      125      126      127      128 
#> 31.10354 35.94313 41.44565 46.15744 36.08221 40.87987 45.73063 51.76033 
#>      129      130      131      132      133      134      135      136 
#> 36.08221 40.87987 45.73063 51.76033 31.10354 35.94313 41.44565 46.15744 
#>      137      138      139      140      141      142      143      144 
#> 36.40827 41.20593 46.05669 52.08639 34.87777 39.67543 44.52619 50.55589 
#>      145      146      147      148      149      150      151      152 
#> 36.74710 41.58669 47.08922 51.80100 31.10354 35.94313 41.44565 46.15744 
#>      153      154      155      156      157      158      159      160 
#> 31.10354 35.94313 41.44565 46.15744 32.30798 37.14756 42.65009 47.36187 
#>      161      162      163      164      165      166      167      168 
#> 32.30798 37.14756 42.65009 47.36187 32.30798 37.14756 42.65009 47.36187 
#>      169      170      171      172      173      174      175      176 
#> 36.40827 41.20593 46.05669 52.08639 36.08221 40.87987 45.73063 51.76033 
#>      177      178      179      180      181      182      183      184 
#> 31.10354 35.94313 41.44565 46.15744 30.77748 35.61706 41.11959 45.83137 
#>      185      186      187      188      189      190      191      192 
#> 34.55171 39.34937 44.20013 50.22983 32.30798 37.14756 42.65009 47.36187 
#>      193      194      195      196      197      198      199      200 
#> 36.74710 41.58669 47.08922 51.80100 34.87777 39.67543 44.52619 50.55589 
#>      201      202      203      204      205      206      207      208 
#> 36.40827 41.20593 46.05669 52.08639 36.40827 41.20593 46.05669 52.08639 
#>      209      210      211      212      213      214      215      216 
#> 36.42104 41.26063 46.76315 51.47494 40.52133 45.31900 50.16976 56.19946 
#>      217      218      219      220      221      222      223      224 
#> 34.55171 39.34937 44.20013 50.22983 32.30798 37.14756 42.65009 47.36187 
#>      225      226      227      228      229      230      231      232 
#> 36.08221 40.87987 45.73063 51.76033 40.52133 45.31900 50.16976 56.19946 
#>      233      234      235      236      237      238      239      240 
#> 40.52133 45.31900 50.16976 56.19946 36.40827 41.20593 46.05669 52.08639 
#>      241      242      243      244      245      246      247      248 
#> 34.55171 39.34937 44.20013 50.22983 31.10354 35.94313 41.44565 46.15744 
#>      249      250      251      252      253      254      255      256 
#> 36.74710 41.58669 47.08922 51.80100 32.63404 37.47363 42.97615 47.68794 
#>      257      258      259      260      261      262      263      264 
#> 34.87777 39.67543 44.52619 50.55589 34.55171 39.34937 44.20013 50.22983 
#>      265      266      267      268      269      270      271      272 
#> 40.52133 45.31900 50.16976 56.19946 40.52133 45.31900 50.16976 56.19946 
#>      273      274      275      276      277      278      279      280 
#> 32.30798 37.14756 42.65009 47.36187 32.30798 37.14756 42.65009 47.36187 
#>      281      282      283      284      285      286      287      288 
#> 34.55171 39.34937 44.20013 50.22983 34.87777 39.67543 44.52619 50.55589 
#>      289      290      291      292      293      294      295      296 
#> 36.42104 41.26063 46.76315 51.47494 36.74710 41.58669 47.08922 51.80100 
#>      297      298      299      300      301      302      303      304 
#> 36.42104 41.26063 46.76315 51.47494 40.19527 44.99293 49.84370 55.87340 
#>      305      306      307      308      309      310      311      312 
#> 34.55171 39.34937 44.20013 50.22983 40.52133 45.31900 50.16976 56.19946 
#>      313      314      315      316      317      318      319      320 
#> 31.10354 35.94313 41.44565 46.15744 34.55171 39.34937 44.20013 50.22983 
#>      321      322      323      324      325      326      327      328 
#> 36.42104 41.26063 46.76315 51.47494 32.63404 37.47363 42.97615 47.68794 
#>      329      330      331      332      333      334      335      336 
#> 40.19527 44.99293 49.84370 55.87340 31.10354 35.94313 41.44565 46.15744 
#>      337      338      339      340      341      342      343      344 
#> 31.10354 35.94313 41.44565 46.15744 36.40827 41.20593 46.05669 52.08639 
#>      345      346      347      348      349      350      351      352 
#> 36.42104 41.26063 46.76315 51.47494 32.63404 37.47363 42.97615 47.68794 
#>      353      354      355      356      357      358      359      360 
#> 32.30798 37.14756 42.65009 47.36187 30.77748 35.61706 41.11959 45.83137 
#>      361      362      363      364      365      366      367      368 
#> 40.52133 45.31900 50.16976 56.19946 40.52133 45.31900 50.16976 56.19946 
#>      369      370      371      372      373      374      375      376 
#> 32.30798 37.14756 42.65009 47.36187 31.10354 35.94313 41.44565 46.15744 
#>      377      378      379      380      381      382      383      384 
#> 31.10354 35.94313 41.44565 46.15744 32.63404 37.47363 42.97615 47.68794 
#>      385      386      387      388      389      390      391      392 
#> 40.19527 44.99293 49.84370 55.87340 30.77748 35.61706 41.11959 45.83137 
#>      393      394      395      396      397      398      399      400 
#> 32.30798 37.14756 42.65009 47.36187 32.63404 37.47363 42.97615 47.68794 
#>      401      402      403      404      405      406      407      408 
#> 40.19527 44.99293 49.84370 55.87340 31.10354 35.94313 41.44565 46.15744 
#>      409      410      411      412      413      414      415      416 
#> 31.10354 35.94313 41.44565 46.15744 32.63404 37.47363 42.97615 47.68794 
#>      417      418      419      420      421      422      423      424 
#> 36.40827 41.20593 46.05669 52.08639 34.87777 39.67543 44.52619 50.55589 
#>      425      426      427      428      429      430      431      432 
#> 36.74710 41.58669 47.08922 51.80100 40.19527 44.99293 49.84370 55.87340 
#>      433      434      435      436      437      438      439      440 
#> 32.63404 37.47363 42.97615 47.68794 34.55171 39.34937 44.20013 50.22983 
#>      441      442      443      444      445      446      447      448 
#> 36.74710 41.58669 47.08922 51.80100 32.63404 37.47363 42.97615 47.68794 
#>      449      450      451      452      453      454      455      456 
#> 30.77748 35.61706 41.11959 45.83137 32.30798 37.14756 42.65009 47.36187 
#>      457      458      459      460      461      462      463      464 
#> 31.10354 35.94313 41.44565 46.15744 36.08221 40.87987 45.73063 51.76033 
#>      465      466      467      468      469      470      471      472 
#> 32.30798 37.14756 42.65009 47.36187 31.10354 35.94313 41.44565 46.15744 
#>      473      474      475      476      477      478      479      480 
#> 40.19527 44.99293 49.84370 55.87340 31.10354 35.94313 41.44565 46.15744 
#>      481      482      483      484      485      486      487      488 
#> 36.08221 40.87987 45.73063 51.76033 31.10354 35.94313 41.44565 46.15744 
#>      489      490      491      492      493      494      495      496 
#> 36.74710 41.58669 47.08922 51.80100 34.87777 39.67543 44.52619 50.55589 
#>      497      498      499      500      501      502      503      504 
#> 36.42104 41.26063 46.76315 51.47494 34.55171 39.34937 44.20013 50.22983 
#>      505      506      507      508      509      510      511      512 
#> 36.40827 41.20593 46.05669 52.08639 34.87777 39.67543 44.52619 50.55589 
#>      513      514      515      516      517      518      519      520 
#> 40.19527 44.99293 49.84370 55.87340 36.42104 41.26063 46.76315 51.47494 
#>      521      522      523      524      525      526      527      528 
#> 31.10354 35.94313 41.44565 46.15744 30.77748 35.61706 41.11959 45.83137 
#>      529      530      531      532      533      534      535      536 
#> 36.42104 41.26063 46.76315 51.47494 32.30798 37.14756 42.65009 47.36187 
#>      537      538      539      540      541      542      543      544 
#> 32.30798 37.14756 42.65009 47.36187 40.52133 45.31900 50.16976 56.19946 
#>      545      546      547      548      549      550      551      552 
#> 34.87777 39.67543 44.52619 50.55589 34.55171 39.34937 44.20013 50.22983 
#>      553      554      555      556      557      558      559      560 
#> 34.87777 39.67543 44.52619 50.55589 32.63404 37.47363 42.97615 47.68794 
#>      561      562      563      564      565      566      567      568 
#> 40.19527 44.99293 49.84370 55.87340 36.08221 40.87987 45.73063 51.76033 
#>      569      570      571      572      573      574      575      576 
#> 36.40827 41.20593 46.05669 52.08639 31.10354 35.94313 41.44565 46.15744 
#>      577      578      579      580      581      582      583      584 
#> 32.30798 37.14756 42.65009 47.36187 34.87777 39.67543 44.52619 50.55589 
#>      585      586      587      588      589      590      591      592 
#> 31.10354 35.94313 41.44565 46.15744 36.08221 40.87987 45.73063 51.76033 
#>      593      594      595      596      597      598      599      600 
#> 31.10354 35.94313 41.44565 46.15744 36.42104 41.26063 46.76315 51.47494 
#>      601      602      603      604      605      606      607      608 
#> 31.10354 35.94313 41.44565 46.15744 34.87777 39.67543 44.52619 50.55589 
#>      609      610      611      612      613      614      615      616 
#> 32.30798 37.14756 42.65009 47.36187 32.63404 37.47363 42.97615 47.68794 
#>      617      618      619      620      621      622      623      624 
#> 34.55171 39.34937 44.20013 50.22983 36.42104 41.26063 46.76315 51.47494 
#>      625      626      627      628      629      630      631      632 
#> 36.08221 40.87987 45.73063 51.76033 36.08221 40.87987 45.73063 51.76033 
#>      633      634      635      636      637      638      639      640 
#> 34.87777 39.67543 44.52619 50.55589 34.55171 39.34937 44.20013 50.22983 
#>      641      642      643      644      645      646      647      648 
#> 32.63404 37.47363 42.97615 47.68794 30.77748 35.61706 41.11959 45.83137 
#>      649      650      651      652      653      654      655      656 
#> 36.42104 41.26063 46.76315 51.47494 32.63404 37.47363 42.97615 47.68794 
#>      657      658      659      660      661      662      663      664 
#> 40.52133 45.31900 50.16976 56.19946 34.55171 39.34937 44.20013 50.22983 
#>      665      666      667      668      669      670      671      672 
#> 32.30798 37.14756 42.65009 47.36187 32.63404 37.47363 42.97615 47.68794 
#>      673      674      675      676      677      678      679      680 
#> 36.74710 41.58669 47.08922 51.80100 40.52133 45.31900 50.16976 56.19946 
#>      681      682      683      684      685      686      687      688 
#> 31.10354 35.94313 41.44565 46.15744 34.87777 39.67543 44.52619 50.55589 
#>      689      690      691      692      693      694      695      696 
#> 40.52133 45.31900 50.16976 56.19946 36.40827 41.20593 46.05669 52.08639 
#>      697      698      699      700      701      702      703      704 
#> 32.63404 37.47363 42.97615 47.68794 36.08221 40.87987 45.73063 51.76033 
#>      705      706      707      708      709      710      711      712 
#> 32.30798 37.14756 42.65009 47.36187 32.30798 37.14756 42.65009 47.36187 
#>      713      714      715      716      717      718      719      720 
#> 40.19527 44.99293 49.84370 55.87340 30.77748 35.61706 41.11959 45.83137 
#>      721      722      723      724      725      726      727      728 
#> 31.10354 35.94313 41.44565 46.15744 36.40827 41.20593 46.05669 52.08639 
#>      729      730      731      732      733      734      735      736 
#> 36.40827 41.20593 46.05669 52.08639 40.52133 45.31900 50.16976 56.19946 
#>      737      738      739      740      741      742      743      744 
#> 32.63404 37.47363 42.97615 47.68794 36.42104 41.26063 46.76315 51.47494 
#>      745      746      747      748      749      750      751      752 
#> 32.63404 37.47363 42.97615 47.68794 32.63404 37.47363 42.97615 47.68794 
#>      753      754      755      756      757      758      759      760 
#> 34.87777 39.67543 44.52619 50.55589 36.40827 41.20593 46.05669 52.08639 
#>      761      762      763      764      765      766      767      768 
#> 40.19527 44.99293 49.84370 55.87340 40.52133 45.31900 50.16976 56.19946 
#>      769      770      771      772      773      774      775      776 
#> 30.77748 35.61706 41.11959 45.83137 32.30798 37.14756 42.65009 47.36187 
#>      777      778      779      780      781      782      783      784 
#> 40.52133 45.31900 50.16976 56.19946 30.77748 35.61706 41.11959 45.83137 
#>      785      786      787      788      789      790      791      792 
#> 32.30798 37.14756 42.65009 47.36187 36.08221 40.87987 45.73063 51.76033 
#>      793      794      795      796      797      798      799      800 
#> 40.19527 44.99293 49.84370 55.87340 32.30798 37.14756 42.65009 47.36187 
# Model frame:
model.frame(object)
#>         FEV1                      RACE    SEX ARMCD AVISIT USUBJID
#> 2   39.97105 Black or African American Female   TRT   VIS2     PT1
#> 4   20.48379 Black or African American Female   TRT   VIS4     PT1
#> 6   31.45522                     Asian   Male   PBO   VIS2     PT2
#> 7   36.87889                     Asian   Male   PBO   VIS3     PT2
#> 8   48.80809                     Asian   Male   PBO   VIS4     PT2
#> 10  35.98699 Black or African American Female   PBO   VIS2     PT3
#> 12  37.16444 Black or African American Female   PBO   VIS4     PT3
#> 13  33.89229                     Asian Female   TRT   VIS1     PT4
#> 14  33.74637                     Asian Female   TRT   VIS2     PT4
#> 16  54.45055                     Asian Female   TRT   VIS4     PT4
#> 17  32.31386 Black or African American   Male   PBO   VIS1     PT5
#> 19  46.79361 Black or African American   Male   PBO   VIS3     PT5
#> 20  41.71154 Black or African American   Male   PBO   VIS4     PT5
#> 23  39.02423 Black or African American   Male   PBO   VIS3     PT6
#> 25  31.93050                     Asian Female   PBO   VIS1     PT7
#> 26  32.90947                     Asian Female   PBO   VIS2     PT7
#> 28  48.28031                     Asian Female   PBO   VIS4     PT7
#> 29  32.23021 Black or African American   Male   PBO   VIS1     PT8
#> 30  35.91080 Black or African American   Male   PBO   VIS2     PT8
#> 31  45.54898 Black or African American   Male   PBO   VIS3     PT8
#> 32  53.02877 Black or African American   Male   PBO   VIS4     PT8
#> 33  47.16898                     White   Male   TRT   VIS1     PT9
#> 34  46.64287                     White   Male   TRT   VIS2     PT9
#> 36  58.09713                     White   Male   TRT   VIS4     PT9
#> 39  44.97613 Black or African American Female   PBO   VIS3    PT10
#> 41  44.32755                     Asian Female   TRT   VIS1    PT11
#> 42  38.97813                     Asian Female   TRT   VIS2    PT11
#> 43  43.72862                     Asian Female   TRT   VIS3    PT11
#> 44  46.43393                     Asian Female   TRT   VIS4    PT11
#> 45  40.34576                     Asian   Male   PBO   VIS1    PT12
#> 46  42.76568                     Asian   Male   PBO   VIS2    PT12
#> 47  40.11155                     Asian   Male   PBO   VIS3    PT12
#> 51  53.31791                     White   Male   TRT   VIS3    PT13
#> 52  56.07641                     White   Male   TRT   VIS4    PT13
#> 55  41.90837 Black or African American   Male   PBO   VIS3    PT14
#> 59  34.65663                     Asian   Male   PBO   VIS3    PT15
#> 60  39.07791                     Asian   Male   PBO   VIS4    PT15
#> 62  35.89612                     Asian Female   PBO   VIS2    PT16
#> 64  47.67264                     Asian Female   PBO   VIS4    PT16
#> 65  22.65440                     White Female   PBO   VIS1    PT17
#> 68  40.85376                     White Female   PBO   VIS4    PT17
#> 69  32.60048                     Asian   Male   TRT   VIS1    PT18
#> 70  33.64329                     Asian   Male   TRT   VIS2    PT18
#> 72  40.92278                     Asian   Male   TRT   VIS4    PT18
#> 73  32.14831                     Asian   Male   TRT   VIS1    PT19
#> 74  46.43604                     Asian   Male   TRT   VIS2    PT19
#> 75  41.34973                     Asian   Male   TRT   VIS3    PT19
#> 76  66.30382                     Asian   Male   TRT   VIS4    PT19
#> 78  47.95358                     White Female   TRT   VIS2    PT20
#> 79  53.97364                     White Female   TRT   VIS3    PT20
#> 82  56.64544                     White   Male   TRT   VIS2    PT21
#> 83  49.70872                     White   Male   TRT   VIS3    PT21
#> 84  60.40497                     White   Male   TRT   VIS4    PT21
#> 85  45.98525                     White   Male   TRT   VIS1    PT22
#> 86  51.90911                     White   Male   TRT   VIS2    PT22
#> 87  41.50787                     White   Male   TRT   VIS3    PT22
#> 88  53.42727                     White   Male   TRT   VIS4    PT22
#> 89  23.86859 Black or African American Female   PBO   VIS1    PT23
#> 90  35.98563 Black or African American Female   PBO   VIS2    PT23
#> 91  43.60626 Black or African American Female   PBO   VIS3    PT23
#> 93  29.59773                     White Female   TRT   VIS1    PT24
#> 94  35.50688                     White Female   TRT   VIS2    PT24
#> 95  55.42944                     White Female   TRT   VIS3    PT24
#> 96  52.10530                     White Female   TRT   VIS4    PT24
#> 97  31.69644                     White Female   TRT   VIS1    PT25
#> 98  32.16159                     White Female   TRT   VIS2    PT25
#> 99  51.04735                     White Female   TRT   VIS3    PT25
#> 100 55.85987                     White Female   TRT   VIS4    PT25
#> 101 49.11706                     White Female   TRT   VIS1    PT26
#> 102 49.25544                     White Female   TRT   VIS2    PT26
#> 103 51.72211                     White Female   TRT   VIS3    PT26
#> 104 69.99128                     White Female   TRT   VIS4    PT26
#> 105 22.07169 Black or African American Female   TRT   VIS1    PT27
#> 107 46.08393 Black or African American Female   TRT   VIS3    PT27
#> 108 52.42288 Black or African American Female   TRT   VIS4    PT27
#> 109 37.69466 Black or African American   Male   TRT   VIS1    PT28
#> 110 44.59400 Black or African American   Male   TRT   VIS2    PT28
#> 111 52.08897 Black or African American   Male   TRT   VIS3    PT28
#> 112 58.22961 Black or African American   Male   TRT   VIS4    PT28
#> 113 37.22824 Black or African American   Male   TRT   VIS1    PT29
#> 114 34.39863 Black or African American   Male   TRT   VIS2    PT29
#> 116 36.34012 Black or African American   Male   TRT   VIS4    PT29
#> 117 45.44182                     Asian Female   TRT   VIS1    PT30
#> 118 41.54847                     Asian Female   TRT   VIS2    PT30
#> 119 43.92172                     Asian Female   TRT   VIS3    PT30
#> 120 61.83243                     Asian Female   TRT   VIS4    PT30
#> 121 27.25656                     Asian Female   PBO   VIS1    PT31
#> 123 45.65133                     Asian Female   PBO   VIS3    PT31
#> 125 33.19334 Black or African American   Male   TRT   VIS1    PT32
#> 128 41.66826 Black or African American   Male   TRT   VIS4    PT32
#> 129 27.12753 Black or African American   Male   TRT   VIS1    PT33
#> 130 31.74858 Black or African American   Male   TRT   VIS2    PT33
#> 132 41.60000 Black or African American   Male   TRT   VIS4    PT33
#> 133 39.45250                     Asian Female   PBO   VIS1    PT34
#> 134 32.61823                     Asian Female   PBO   VIS2    PT34
#> 135 34.62445                     Asian Female   PBO   VIS3    PT34
#> 136 45.90515                     Asian Female   PBO   VIS4    PT34
#> 137 36.17780 Black or African American Female   TRT   VIS1    PT35
#> 138 39.79796 Black or African American Female   TRT   VIS2    PT35
#> 140 50.08272 Black or African American Female   TRT   VIS4    PT35
#> 142 44.64316                     Asian Female   TRT   VIS2    PT36
#> 144 39.73529                     Asian Female   TRT   VIS4    PT36
#> 145 34.06164                     White Female   PBO   VIS1    PT37
#> 146 40.18592                     White Female   PBO   VIS2    PT37
#> 147 41.17584                     White Female   PBO   VIS3    PT37
#> 148 57.76669                     White Female   PBO   VIS4    PT37
#> 149 38.18460                     Asian Female   PBO   VIS1    PT38
#> 151 47.19893                     Asian Female   PBO   VIS3    PT38
#> 153 37.32785                     Asian Female   PBO   VIS1    PT39
#> 155 43.16048                     Asian Female   PBO   VIS3    PT39
#> 156 41.40349                     Asian Female   PBO   VIS4    PT39
#> 157 30.15733 Black or African American   Male   PBO   VIS1    PT40
#> 158 35.84353 Black or African American   Male   PBO   VIS2    PT40
#> 159 40.95250 Black or African American   Male   PBO   VIS3    PT40
#> 162 41.37928 Black or African American   Male   PBO   VIS2    PT41
#> 163 50.17316 Black or African American   Male   PBO   VIS3    PT41
#> 164 45.35226 Black or African American   Male   PBO   VIS4    PT41
#> 165 39.06491 Black or African American   Male   PBO   VIS1    PT42
#> 168 42.11960 Black or African American   Male   PBO   VIS4    PT42
#> 169 29.81042 Black or African American Female   TRT   VIS1    PT43
#> 170 42.57055 Black or African American Female   TRT   VIS2    PT43
#> 171 47.81652 Black or African American Female   TRT   VIS3    PT43
#> 172 68.06024 Black or African American Female   TRT   VIS4    PT43
#> 173 35.62071 Black or African American   Male   TRT   VIS1    PT44
#> 177 33.89134                     Asian Female   PBO   VIS1    PT45
#> 178 36.42808                     Asian Female   PBO   VIS2    PT45
#> 179 37.57519                     Asian Female   PBO   VIS3    PT45
#> 180 58.46873                     Asian Female   PBO   VIS4    PT45
#> 181 19.54516                     Asian   Male   PBO   VIS1    PT46
#> 182 31.13541                     Asian   Male   PBO   VIS2    PT46
#> 183 40.89955                     Asian   Male   PBO   VIS3    PT46
#> 185 22.18809                     Asian   Male   TRT   VIS1    PT47
#> 186 41.05857                     Asian   Male   TRT   VIS2    PT47
#> 187 37.32452                     Asian   Male   TRT   VIS3    PT47
#> 190 43.12432 Black or African American   Male   PBO   VIS2    PT48
#> 191 41.99349 Black or African American   Male   PBO   VIS3    PT48
#> 193 44.03080                     White Female   PBO   VIS1    PT49
#> 194 38.66417                     White Female   PBO   VIS2    PT49
#> 195 53.45993                     White Female   PBO   VIS3    PT49
#> 197 29.81948                     Asian Female   TRT   VIS1    PT50
#> 198 30.43859                     Asian Female   TRT   VIS2    PT50
#> 199 40.18095                     Asian Female   TRT   VIS3    PT50
#> 201 26.78578 Black or African American Female   TRT   VIS1    PT51
#> 202 34.55115 Black or African American Female   TRT   VIS2    PT51
#> 204 40.06421 Black or African American Female   TRT   VIS4    PT51
#> 206 43.09329 Black or African American Female   TRT   VIS2    PT52
#> 208 45.71567 Black or African American Female   TRT   VIS4    PT52
#> 209 40.74992                     White   Male   PBO   VIS1    PT53
#> 210 44.74635                     White   Male   PBO   VIS2    PT53
#> 217 40.14674                     Asian   Male   TRT   VIS1    PT55
#> 218 48.75859                     Asian   Male   TRT   VIS2    PT55
#> 219 46.43462                     Asian   Male   TRT   VIS3    PT55
#> 221 29.33990 Black or African American   Male   PBO   VIS1    PT56
#> 224 47.93165 Black or African American   Male   PBO   VIS4    PT56
#> 226 41.11632 Black or African American   Male   TRT   VIS2    PT57
#> 227 47.05889 Black or African American   Male   TRT   VIS3    PT57
#> 228 52.24599 Black or African American   Male   TRT   VIS4    PT57
#> 230 54.14236                     White Female   TRT   VIS2    PT58
#> 231 50.44618                     White Female   TRT   VIS3    PT58
#> 233 37.53657                     White Female   TRT   VIS1    PT59
#> 235 49.45840                     White Female   TRT   VIS3    PT59
#> 236 59.12866                     White Female   TRT   VIS4    PT59
#> 237 40.31268 Black or African American Female   TRT   VIS1    PT60
#> 238 39.66049 Black or African American Female   TRT   VIS2    PT60
#> 239 50.89726 Black or African American Female   TRT   VIS3    PT60
#> 240 56.13116 Black or African American Female   TRT   VIS4    PT60
#> 241 32.82981                     Asian   Male   TRT   VIS1    PT61
#> 242 46.53837                     Asian   Male   TRT   VIS2    PT61
#> 244 51.81265                     Asian   Male   TRT   VIS4    PT61
#> 246 29.91939                     Asian Female   PBO   VIS2    PT62
#> 250 51.05656                     White Female   PBO   VIS2    PT63
#> 251 50.50059                     White Female   PBO   VIS3    PT63
#> 252 64.11388                     White Female   PBO   VIS4    PT63
#> 253 32.21843 Black or African American Female   PBO   VIS1    PT64
#> 254 29.64732 Black or African American Female   PBO   VIS2    PT64
#> 256 45.09919 Black or African American Female   PBO   VIS4    PT64
#> 257 39.75659                     Asian Female   TRT   VIS1    PT65
#> 258 37.28894                     Asian Female   TRT   VIS2    PT65
#> 259 44.80145                     Asian Female   TRT   VIS3    PT65
#> 260 65.95920                     Asian Female   TRT   VIS4    PT65
#> 261 33.43439                     Asian   Male   TRT   VIS1    PT66
#> 262 33.57042                     Asian   Male   TRT   VIS2    PT66
#> 263 39.91543                     Asian   Male   TRT   VIS3    PT66
#> 264 49.57098                     Asian   Male   TRT   VIS4    PT66
#> 265 38.91634                     White Female   TRT   VIS1    PT67
#> 266 36.69011                     White Female   TRT   VIS2    PT67
#> 267 45.66665                     White Female   TRT   VIS3    PT67
#> 268 52.07431                     White Female   TRT   VIS4    PT67
#> 269 42.21411                     White Female   TRT   VIS1    PT68
#> 270 45.02901                     White Female   TRT   VIS2    PT68
#> 273 30.98338 Black or African American   Male   PBO   VIS1    PT69
#> 274 44.72932 Black or African American   Male   PBO   VIS2    PT69
#> 275 40.68711 Black or African American   Male   PBO   VIS3    PT69
#> 276 34.71530 Black or African American   Male   PBO   VIS4    PT69
#> 277 27.30752 Black or African American   Male   PBO   VIS1    PT70
#> 278 37.31585 Black or African American   Male   PBO   VIS2    PT70
#> 280 44.83000 Black or African American   Male   PBO   VIS4    PT70
#> 281 32.93042                     Asian   Male   TRT   VIS1    PT71
#> 282 44.91911                     Asian   Male   TRT   VIS2    PT71
#> 283 45.68636                     Asian   Male   TRT   VIS3    PT71
#> 284 65.98800                     Asian   Male   TRT   VIS4    PT71
#> 285 46.60130                     Asian Female   TRT   VIS1    PT72
#> 286 40.89786                     Asian Female   TRT   VIS2    PT72
#> 287 46.66708                     Asian Female   TRT   VIS3    PT72
#> 291 43.83270                     White   Male   PBO   VIS3    PT73
#> 292 44.11604                     White   Male   PBO   VIS4    PT73
#> 293 38.29612                     White Female   PBO   VIS1    PT74
#> 295 51.38570                     White Female   PBO   VIS3    PT74
#> 296 56.20979                     White Female   PBO   VIS4    PT74
#> 298 43.45819                     White   Male   PBO   VIS2    PT75
#> 299 38.38741                     White   Male   PBO   VIS3    PT75
#> 300 56.42818                     White   Male   PBO   VIS4    PT75
#> 301 39.05050                     White   Male   TRT   VIS1    PT76
#> 304 54.09200                     White   Male   TRT   VIS4    PT76
#> 305 31.40521                     Asian   Male   TRT   VIS1    PT77
#> 306 46.13330                     Asian   Male   TRT   VIS2    PT77
#> 307 45.29845                     Asian   Male   TRT   VIS3    PT77
#> 308 28.06936                     Asian   Male   TRT   VIS4    PT77
#> 310 42.50283                     White Female   TRT   VIS2    PT78
#> 311 46.45368                     White Female   TRT   VIS3    PT78
#> 312 64.97366                     White Female   TRT   VIS4    PT78
#> 316 43.97847                     Asian Female   PBO   VIS4    PT79
#> 317 35.33466                     Asian   Male   TRT   VIS1    PT80
#> 318 39.34378                     Asian   Male   TRT   VIS2    PT80
#> 319 41.27633                     Asian   Male   TRT   VIS3    PT80
#> 322 39.83058                     White   Male   PBO   VIS2    PT81
#> 323 43.49673                     White   Male   PBO   VIS3    PT81
#> 324 44.06114                     White   Male   PBO   VIS4    PT81
#> 325 41.43742 Black or African American Female   PBO   VIS1    PT82
#> 327 46.16954 Black or African American Female   PBO   VIS3    PT82
#> 328 54.24024 Black or African American Female   PBO   VIS4    PT82
#> 329 36.61831                     White   Male   TRT   VIS1    PT83
#> 330 42.09272                     White   Male   TRT   VIS2    PT83
#> 331 50.69556                     White   Male   TRT   VIS3    PT83
#> 332 51.72563                     White   Male   TRT   VIS4    PT83
#> 336 53.89947                     Asian Female   PBO   VIS4    PT84
#> 339 39.94420                     Asian Female   PBO   VIS3    PT85
#> 340 56.42482                     Asian Female   PBO   VIS4    PT85
#> 341 41.86385 Black or African American Female   TRT   VIS1    PT86
#> 342 34.56420 Black or African American Female   TRT   VIS2    PT86
#> 343 38.68927 Black or African American Female   TRT   VIS3    PT86
#> 344 62.88743 Black or African American Female   TRT   VIS4    PT86
#> 345 28.85343                     White   Male   PBO   VIS1    PT87
#> 347 49.29495                     White   Male   PBO   VIS3    PT87
#> 349 28.74029 Black or African American Female   PBO   VIS1    PT88
#> 351 43.59994 Black or African American Female   PBO   VIS3    PT88
#> 352 57.38616 Black or African American Female   PBO   VIS4    PT88
#> 353 35.36824 Black or African American   Male   PBO   VIS1    PT89
#> 354 43.06110 Black or African American   Male   PBO   VIS2    PT89
#> 355 31.27551 Black or African American   Male   PBO   VIS3    PT89
#> 356 54.13245 Black or African American   Male   PBO   VIS4    PT89
#> 357 25.97050                     Asian   Male   PBO   VIS1    PT90
#> 363 51.17493                     White Female   TRT   VIS3    PT91
#> 364 48.44043                     White Female   TRT   VIS4    PT91
#> 365 43.33128                     White Female   TRT   VIS1    PT92
#> 367 55.93546                     White Female   TRT   VIS3    PT92
#> 368 54.15312                     White Female   TRT   VIS4    PT92
#> 370 40.60252 Black or African American   Male   PBO   VIS2    PT93
#> 371 44.44715 Black or African American   Male   PBO   VIS3    PT93
#> 372 40.54161 Black or African American   Male   PBO   VIS4    PT93
#> 373 33.95563                     Asian Female   PBO   VIS1    PT94
#> 375 43.67802                     Asian Female   PBO   VIS3    PT94
#> 376 42.76023                     Asian Female   PBO   VIS4    PT94
#> 378 42.82678                     Asian Female   PBO   VIS2    PT95
#> 379 39.59218                     Asian Female   PBO   VIS3    PT95
#> 381 33.49216 Black or African American Female   PBO   VIS1    PT96
#> 382 35.39266 Black or African American Female   PBO   VIS2    PT96
#> 384 42.36266 Black or African American Female   PBO   VIS4    PT96
#> 385 48.54368                     White   Male   TRT   VIS1    PT97
#> 386 43.94366                     White   Male   TRT   VIS2    PT97
#> 388 47.91204                     White   Male   TRT   VIS4    PT97
#> 389 20.72928                     Asian   Male   PBO   VIS1    PT98
#> 390 28.00599                     Asian   Male   PBO   VIS2    PT98
#> 391 40.19255                     Asian   Male   PBO   VIS3    PT98
#> 392 37.79360                     Asian   Male   PBO   VIS4    PT98
#> 394 36.75177 Black or African American   Male   PBO   VIS2    PT99
#> 397 34.59822 Black or African American Female   PBO   VIS1   PT100
#> 398 39.32034 Black or African American Female   PBO   VIS2   PT100
#> 399 40.65702 Black or African American Female   PBO   VIS3   PT100
#> 402 43.03255                     White   Male   TRT   VIS2   PT101
#> 403 54.65715                     White   Male   TRT   VIS3   PT101
#> 405 35.55742                     Asian Female   PBO   VIS1   PT102
#> 406 43.70215                     Asian Female   PBO   VIS2   PT102
#> 407 42.52157                     Asian Female   PBO   VIS3   PT102
#> 408 54.89337                     Asian Female   PBO   VIS4   PT102
#> 409 32.03460                     Asian Female   PBO   VIS1   PT103
#> 410 29.45107                     Asian Female   PBO   VIS2   PT103
#> 411 45.35138                     Asian Female   PBO   VIS3   PT103
#> 413 38.73784 Black or African American Female   PBO   VIS1   PT104
#> 415 41.42283 Black or African American Female   PBO   VIS3   PT104
#> 416 47.32385 Black or African American Female   PBO   VIS4   PT104
#> 418 47.55310 Black or African American Female   TRT   VIS2   PT105
#> 419 49.06509 Black or African American Female   TRT   VIS3   PT105
#> 421 29.22591                     Asian Female   TRT   VIS1   PT106
#> 422 40.08175                     Asian Female   TRT   VIS2   PT106
#> 423 45.68142                     Asian Female   TRT   VIS3   PT106
#> 424 41.47403                     Asian Female   TRT   VIS4   PT106
#> 427 42.51970                     White Female   PBO   VIS3   PT107
#> 428 69.36099                     White Female   PBO   VIS4   PT107
#> 429 42.39760                     White   Male   TRT   VIS1   PT108
#> 430 43.72376                     White   Male   TRT   VIS2   PT108
#> 431 49.47601                     White   Male   TRT   VIS3   PT108
#> 432 51.94188                     White   Male   TRT   VIS4   PT108
#> 434 40.59100 Black or African American Female   PBO   VIS2   PT109
#> 435 39.97833 Black or African American Female   PBO   VIS3   PT109
#> 436 31.69049 Black or African American Female   PBO   VIS4   PT109
#> 438 37.20517                     Asian   Male   TRT   VIS2   PT110
#> 439 46.28740                     Asian   Male   TRT   VIS3   PT110
#> 444 41.58720                     White Female   PBO   VIS4   PT111
#> 445 32.17365 Black or African American Female   PBO   VIS1   PT112
#> 447 40.69375 Black or African American Female   PBO   VIS3   PT112
#> 449 32.28771                     Asian   Male   PBO   VIS1   PT113
#> 450 41.76205                     Asian   Male   PBO   VIS2   PT113
#> 451 40.06768                     Asian   Male   PBO   VIS3   PT113
#> 453 29.14213 Black or African American   Male   PBO   VIS1   PT114
#> 454 39.50989 Black or African American   Male   PBO   VIS2   PT114
#> 455 43.32349 Black or African American   Male   PBO   VIS3   PT114
#> 456 47.16756 Black or African American   Male   PBO   VIS4   PT114
#> 457 40.93020                     Asian Female   PBO   VIS1   PT115
#> 458 42.19406                     Asian Female   PBO   VIS2   PT115
#> 459 41.21057                     Asian Female   PBO   VIS3   PT115
#> 461 38.54330 Black or African American   Male   TRT   VIS1   PT116
#> 463 43.96324 Black or African American   Male   TRT   VIS3   PT116
#> 464 42.67652 Black or African American   Male   TRT   VIS4   PT116
#> 465 22.79584 Black or African American   Male   PBO   VIS1   PT117
#> 469 31.43559                     Asian Female   PBO   VIS1   PT118
#> 470 38.85064                     Asian Female   PBO   VIS2   PT118
#> 471 48.24288                     Asian Female   PBO   VIS3   PT118
#> 473 44.71302                     White   Male   TRT   VIS1   PT119
#> 474 51.85370                     White   Male   TRT   VIS2   PT119
#> 477 30.56757                     Asian Female   PBO   VIS1   PT120
#> 484 59.90473 Black or African American   Male   TRT   VIS4   PT121
#> 487 49.76150                     Asian Female   PBO   VIS3   PT122
#> 489 47.21985                     White Female   PBO   VIS1   PT123
#> 490 40.34525                     White Female   PBO   VIS2   PT123
#> 491 48.29793                     White Female   PBO   VIS3   PT123
#> 494 44.39634                     Asian Female   TRT   VIS2   PT124
#> 495 41.71421                     Asian Female   TRT   VIS3   PT124
#> 496 47.37535                     Asian Female   TRT   VIS4   PT124
#> 497 42.03797                     White   Male   PBO   VIS1   PT125
#> 498 37.56100                     White   Male   PBO   VIS2   PT125
#> 499 45.11793                     White   Male   PBO   VIS3   PT125
#> 501 34.62530                     Asian   Male   TRT   VIS1   PT126
#> 502 45.28206                     Asian   Male   TRT   VIS2   PT126
#> 504 63.57761                     Asian   Male   TRT   VIS4   PT126
#> 505 35.80878 Black or African American Female   TRT   VIS1   PT127
#> 508 52.67314 Black or African American Female   TRT   VIS4   PT127
#> 509 35.88734                     Asian Female   TRT   VIS1   PT128
#> 510 38.73222                     Asian Female   TRT   VIS2   PT128
#> 511 46.70361                     Asian Female   TRT   VIS3   PT128
#> 512 53.65398                     Asian Female   TRT   VIS4   PT128
#> 513 36.71543                     White   Male   TRT   VIS1   PT129
#> 518 41.54317                     White   Male   PBO   VIS2   PT130
#> 519 51.67909                     White   Male   PBO   VIS3   PT130
#> 521 27.40130                     Asian Female   PBO   VIS1   PT131
#> 522 30.33517                     Asian Female   PBO   VIS2   PT131
#> 523 37.73092                     Asian Female   PBO   VIS3   PT131
#> 524 29.11668                     Asian Female   PBO   VIS4   PT131
#> 526 32.08830                     Asian   Male   PBO   VIS2   PT132
#> 527 41.66067                     Asian   Male   PBO   VIS3   PT132
#> 528 53.90815                     Asian   Male   PBO   VIS4   PT132
#> 530 35.06937                     White   Male   PBO   VIS2   PT133
#> 531 47.17615                     White   Male   PBO   VIS3   PT133
#> 532 56.49347                     White   Male   PBO   VIS4   PT133
#> 534 38.88006 Black or African American   Male   PBO   VIS2   PT134
#> 535 47.54070 Black or African American   Male   PBO   VIS3   PT134
#> 536 43.53705 Black or African American   Male   PBO   VIS4   PT134
#> 537 31.82054 Black or African American   Male   PBO   VIS1   PT135
#> 538 39.62816 Black or African American   Male   PBO   VIS2   PT135
#> 539 44.95543 Black or African American   Male   PBO   VIS3   PT135
#> 540 21.11543 Black or African American   Male   PBO   VIS4   PT135
#> 541 34.74671                     White Female   TRT   VIS1   PT136
#> 544 56.69249                     White Female   TRT   VIS4   PT136
#> 545 22.73126                     Asian Female   TRT   VIS1   PT137
#> 546 32.50075                     Asian Female   TRT   VIS2   PT137
#> 547 42.37206                     Asian Female   TRT   VIS3   PT137
#> 548 42.89847                     Asian Female   TRT   VIS4   PT137
#> 549 55.62582                     Asian   Male   TRT   VIS1   PT138
#> 550 45.38998                     Asian   Male   TRT   VIS2   PT138
#> 551 52.66743                     Asian   Male   TRT   VIS3   PT138
#> 555 34.18931                     Asian Female   TRT   VIS3   PT139
#> 556 45.59740                     Asian Female   TRT   VIS4   PT139
#> 557 28.89198 Black or African American Female   PBO   VIS1   PT140
#> 558 38.46147 Black or African American Female   PBO   VIS2   PT140
#> 560 49.90357 Black or African American Female   PBO   VIS4   PT140
#> 562 44.14167                     White   Male   TRT   VIS2   PT141
#> 564 55.24278                     White   Male   TRT   VIS4   PT141
#> 569 27.38001 Black or African American Female   TRT   VIS1   PT143
#> 570 33.63251 Black or African American Female   TRT   VIS2   PT143
#> 572 39.34410 Black or African American Female   TRT   VIS4   PT143
#> 573 26.98575                     Asian Female   PBO   VIS1   PT144
#> 574 24.04175                     Asian Female   PBO   VIS2   PT144
#> 575 42.16648                     Asian Female   PBO   VIS3   PT144
#> 576 44.75380                     Asian Female   PBO   VIS4   PT144
#> 577 31.55469 Black or African American   Male   PBO   VIS1   PT145
#> 578 44.42696 Black or African American   Male   PBO   VIS2   PT145
#> 579 44.10343 Black or African American   Male   PBO   VIS3   PT145
#> 582 37.87445                     Asian Female   TRT   VIS2   PT146
#> 583 48.31828                     Asian Female   TRT   VIS3   PT146
#> 584 50.21520                     Asian Female   TRT   VIS4   PT146
#> 585 41.94615                     Asian Female   PBO   VIS1   PT147
#> 586 39.62690                     Asian Female   PBO   VIS2   PT147
#> 587 46.69763                     Asian Female   PBO   VIS3   PT147
#> 590 43.75255 Black or African American   Male   TRT   VIS2   PT148
#> 591 47.38873 Black or African American   Male   TRT   VIS3   PT148
#> 593 32.43412                     Asian Female   PBO   VIS1   PT149
#> 594 43.07163                     Asian Female   PBO   VIS2   PT149
#> 595 42.99551                     Asian Female   PBO   VIS3   PT149
#> 596 53.82759                     Asian Female   PBO   VIS4   PT149
#> 599 50.64802                     White   Male   PBO   VIS3   PT150
#> 600 63.44051                     White   Male   PBO   VIS4   PT150
#> 601 34.48949                     Asian Female   PBO   VIS1   PT151
#> 602 40.08056                     Asian Female   PBO   VIS2   PT151
#> 604 47.46553                     Asian Female   PBO   VIS4   PT151
#> 606 37.11697                     Asian Female   TRT   VIS2   PT152
#> 608 36.25120                     Asian Female   TRT   VIS4   PT152
#> 609 29.20171 Black or African American   Male   PBO   VIS1   PT153
#> 610 31.53773 Black or African American   Male   PBO   VIS2   PT153
#> 611 42.35683 Black or African American   Male   PBO   VIS3   PT153
#> 612 64.78352 Black or African American   Male   PBO   VIS4   PT153
#> 613 32.72757 Black or African American Female   PBO   VIS1   PT154
#> 614 37.50022 Black or African American Female   PBO   VIS2   PT154
#> 616 57.03861 Black or African American Female   PBO   VIS4   PT154
#> 617 36.32475                     Asian   Male   TRT   VIS1   PT155
#> 619 41.46725                     Asian   Male   TRT   VIS3   PT155
#> 620 59.01411                     Asian   Male   TRT   VIS4   PT155
#> 621 30.14970                     White   Male   PBO   VIS1   PT156
#> 622 34.91740                     White   Male   PBO   VIS2   PT156
#> 623 52.13900                     White   Male   PBO   VIS3   PT156
#> 624 58.73839                     White   Male   PBO   VIS4   PT156
#> 625 35.83185 Black or African American   Male   TRT   VIS1   PT157
#> 628 56.41409 Black or African American   Male   TRT   VIS4   PT157
#> 630 43.55593 Black or African American   Male   TRT   VIS2   PT158
#> 631 44.26320 Black or African American   Male   TRT   VIS3   PT158
#> 632 59.25579 Black or African American   Male   TRT   VIS4   PT158
#> 633 28.47314                     Asian Female   TRT   VIS1   PT159
#> 634 47.47581                     Asian Female   TRT   VIS2   PT159
#> 638 46.47483                     Asian   Male   TRT   VIS2   PT160
#> 639 51.22677                     Asian   Male   TRT   VIS3   PT160
#> 640 45.82777                     Asian   Male   TRT   VIS4   PT160
#> 642 39.06783 Black or African American Female   PBO   VIS2   PT161
#> 645 29.99542                     Asian   Male   PBO   VIS1   PT162
#> 648 54.17796                     Asian   Male   PBO   VIS4   PT162
#> 650 44.55743                     White   Male   PBO   VIS2   PT163
#> 652 62.59579                     White   Male   PBO   VIS4   PT163
#> 654 35.48396 Black or African American Female   PBO   VIS2   PT164
#> 655 44.07768 Black or African American Female   PBO   VIS3   PT164
#> 656 46.57837 Black or African American Female   PBO   VIS4   PT164
#> 657 47.67979                     White Female   TRT   VIS1   PT165
#> 661 22.15439                     Asian   Male   TRT   VIS1   PT166
#> 665 34.27765 Black or African American   Male   PBO   VIS1   PT167
#> 666 36.90059 Black or African American   Male   PBO   VIS2   PT167
#> 668 40.54285 Black or African American   Male   PBO   VIS4   PT167
#> 669 29.09494 Black or African American Female   PBO   VIS1   PT168
#> 670 37.21768 Black or African American Female   PBO   VIS2   PT168
#> 671 43.08491 Black or African American Female   PBO   VIS3   PT168
#> 673 27.12174                     White Female   PBO   VIS1   PT169
#> 674 34.11916                     White Female   PBO   VIS2   PT169
#> 678 40.80230                     White Female   TRT   VIS2   PT170
#> 679 45.89269                     White Female   TRT   VIS3   PT170
#> 680 43.69153                     White Female   TRT   VIS4   PT170
#> 682 29.22869                     Asian Female   PBO   VIS2   PT171
#> 684 55.68362                     Asian Female   PBO   VIS4   PT171
#> 685 31.90698                     Asian Female   TRT   VIS1   PT172
#> 686 37.31061                     Asian Female   TRT   VIS2   PT172
#> 687 40.75546                     Asian Female   TRT   VIS3   PT172
#> 689 42.19474                     White Female   TRT   VIS1   PT173
#> 690 44.87228                     White Female   TRT   VIS2   PT173
#> 691 47.55198                     White Female   TRT   VIS3   PT173
#> 693 50.62894 Black or African American Female   TRT   VIS1   PT174
#> 694 45.47551 Black or African American Female   TRT   VIS2   PT174
#> 695 48.62168 Black or African American Female   TRT   VIS3   PT174
#> 697 29.66493 Black or African American Female   PBO   VIS1   PT175
#> 698 34.57406 Black or African American Female   PBO   VIS2   PT175
#> 700 38.11676 Black or African American Female   PBO   VIS4   PT175
#> 701 33.77204 Black or African American   Male   TRT   VIS1   PT176
#> 702 34.26148 Black or African American   Male   TRT   VIS2   PT176
#> 704 58.81037 Black or African American   Male   TRT   VIS4   PT176
#> 707 39.88119 Black or African American   Male   PBO   VIS3   PT177
#> 709 31.62708 Black or African American   Male   PBO   VIS1   PT178
#> 712 48.22049 Black or African American   Male   PBO   VIS4   PT178
#> 713 42.58829                     White   Male   TRT   VIS1   PT179
#> 715 49.33262                     White   Male   TRT   VIS3   PT179
#> 716 53.74331                     White   Male   TRT   VIS4   PT179
#> 717 29.71857                     Asian   Male   PBO   VIS1   PT180
#> 718 30.45651                     Asian   Male   PBO   VIS2   PT180
#> 719 38.29800                     Asian   Male   PBO   VIS3   PT180
#> 721 36.81040                     Asian Female   PBO   VIS1   PT181
#> 723 42.35045                     Asian Female   PBO   VIS3   PT181
#> 724 39.39860                     Asian Female   PBO   VIS4   PT181
#> 727 49.73629 Black or African American Female   TRT   VIS3   PT182
#> 728 41.58082 Black or African American Female   TRT   VIS4   PT182
#> 729 43.58901 Black or African American Female   TRT   VIS1   PT183
#> 730 40.16762 Black or African American Female   TRT   VIS2   PT183
#> 734 41.08206                     White Female   TRT   VIS2   PT184
#> 736 69.37409                     White Female   TRT   VIS4   PT184
#> 738 41.27625 Black or African American Female   PBO   VIS2   PT185
#> 739 44.76138 Black or African American Female   PBO   VIS3   PT185
#> 740 39.69815 Black or African American Female   PBO   VIS4   PT185
#> 741 38.44296                     White   Male   PBO   VIS1   PT186
#> 742 48.20586                     White   Male   PBO   VIS2   PT186
#> 744 35.50735                     White   Male   PBO   VIS4   PT186
#> 745 32.08153 Black or African American Female   PBO   VIS1   PT187
#> 749 44.69256 Black or African American Female   PBO   VIS1   PT188
#> 751 42.18689 Black or African American Female   PBO   VIS3   PT188
#> 753 37.01741                     Asian Female   TRT   VIS1   PT189
#> 754 38.26920                     Asian Female   TRT   VIS2   PT189
#> 755 49.28806                     Asian Female   TRT   VIS3   PT189
#> 757 40.45953 Black or African American Female   TRT   VIS1   PT190
#> 758 45.10337 Black or African American Female   TRT   VIS2   PT190
#> 759 45.58250 Black or African American Female   TRT   VIS3   PT190
#> 760 62.96989 Black or African American Female   TRT   VIS4   PT190
#> 761 30.78252                     White   Male   TRT   VIS1   PT191
#> 764 44.69667                     White   Male   TRT   VIS4   PT191
#> 765 32.72491                     White Female   TRT   VIS1   PT192
#> 766 45.78702                     White Female   TRT   VIS2   PT192
#> 767 48.74886                     White Female   TRT   VIS3   PT192
#> 768 84.08449                     White Female   TRT   VIS4   PT192
#> 770 30.19495                     Asian   Male   PBO   VIS2   PT193
#> 771 36.78573                     Asian   Male   PBO   VIS3   PT193
#> 772 61.03588                     Asian   Male   PBO   VIS4   PT193
#> 773 20.36749 Black or African American   Male   PBO   VIS1   PT194
#> 774 35.22480 Black or African American   Male   PBO   VIS2   PT194
#> 775 37.42847 Black or African American   Male   PBO   VIS3   PT194
#> 776 30.20501 Black or African American   Male   PBO   VIS4   PT194
#> 778 49.12862                     White Female   TRT   VIS2   PT195
#> 779 47.31234                     White Female   TRT   VIS3   PT195
#> 781 19.28388                     Asian   Male   PBO   VIS1   PT196
#> 782 30.00682                     Asian   Male   PBO   VIS2   PT196
#> 784 49.21768                     Asian   Male   PBO   VIS4   PT196
#> 788 40.13353 Black or African American   Male   PBO   VIS4   PT197
#> 789 42.34534 Black or African American   Male   TRT   VIS1   PT198
#> 790 52.32575 Black or African American   Male   TRT   VIS2   PT198
#> 792 69.26254 Black or African American   Male   TRT   VIS4   PT198
#> 797 35.70341 Black or African American   Male   PBO   VIS1   PT200
#> 798 41.64454 Black or African American   Male   PBO   VIS2   PT200
#> 800 54.25081 Black or African American   Male   PBO   VIS4   PT200
model.frame(object, include = "subject_var")
#>                          RACE    SEX ARMCD AVISIT USUBJID
#> 1   Black or African American Female   TRT   VIS1     PT1
#> 2   Black or African American Female   TRT   VIS2     PT1
#> 3   Black or African American Female   TRT   VIS3     PT1
#> 4   Black or African American Female   TRT   VIS4     PT1
#> 5                       Asian   Male   PBO   VIS1     PT2
#> 6                       Asian   Male   PBO   VIS2     PT2
#> 7                       Asian   Male   PBO   VIS3     PT2
#> 8                       Asian   Male   PBO   VIS4     PT2
#> 9   Black or African American Female   PBO   VIS1     PT3
#> 10  Black or African American Female   PBO   VIS2     PT3
#> 11  Black or African American Female   PBO   VIS3     PT3
#> 12  Black or African American Female   PBO   VIS4     PT3
#> 13                      Asian Female   TRT   VIS1     PT4
#> 14                      Asian Female   TRT   VIS2     PT4
#> 15                      Asian Female   TRT   VIS3     PT4
#> 16                      Asian Female   TRT   VIS4     PT4
#> 17  Black or African American   Male   PBO   VIS1     PT5
#> 18  Black or African American   Male   PBO   VIS2     PT5
#> 19  Black or African American   Male   PBO   VIS3     PT5
#> 20  Black or African American   Male   PBO   VIS4     PT5
#> 21  Black or African American   Male   PBO   VIS1     PT6
#> 22  Black or African American   Male   PBO   VIS2     PT6
#> 23  Black or African American   Male   PBO   VIS3     PT6
#> 24  Black or African American   Male   PBO   VIS4     PT6
#> 25                      Asian Female   PBO   VIS1     PT7
#> 26                      Asian Female   PBO   VIS2     PT7
#> 27                      Asian Female   PBO   VIS3     PT7
#> 28                      Asian Female   PBO   VIS4     PT7
#> 29  Black or African American   Male   PBO   VIS1     PT8
#> 30  Black or African American   Male   PBO   VIS2     PT8
#> 31  Black or African American   Male   PBO   VIS3     PT8
#> 32  Black or African American   Male   PBO   VIS4     PT8
#> 33                      White   Male   TRT   VIS1     PT9
#> 34                      White   Male   TRT   VIS2     PT9
#> 35                      White   Male   TRT   VIS3     PT9
#> 36                      White   Male   TRT   VIS4     PT9
#> 37  Black or African American Female   PBO   VIS1    PT10
#> 38  Black or African American Female   PBO   VIS2    PT10
#> 39  Black or African American Female   PBO   VIS3    PT10
#> 40  Black or African American Female   PBO   VIS4    PT10
#> 41                      Asian Female   TRT   VIS1    PT11
#> 42                      Asian Female   TRT   VIS2    PT11
#> 43                      Asian Female   TRT   VIS3    PT11
#> 44                      Asian Female   TRT   VIS4    PT11
#> 45                      Asian   Male   PBO   VIS1    PT12
#> 46                      Asian   Male   PBO   VIS2    PT12
#> 47                      Asian   Male   PBO   VIS3    PT12
#> 48                      Asian   Male   PBO   VIS4    PT12
#> 49                      White   Male   TRT   VIS1    PT13
#> 50                      White   Male   TRT   VIS2    PT13
#> 51                      White   Male   TRT   VIS3    PT13
#> 52                      White   Male   TRT   VIS4    PT13
#> 53  Black or African American   Male   PBO   VIS1    PT14
#> 54  Black or African American   Male   PBO   VIS2    PT14
#> 55  Black or African American   Male   PBO   VIS3    PT14
#> 56  Black or African American   Male   PBO   VIS4    PT14
#> 57                      Asian   Male   PBO   VIS1    PT15
#> 58                      Asian   Male   PBO   VIS2    PT15
#> 59                      Asian   Male   PBO   VIS3    PT15
#> 60                      Asian   Male   PBO   VIS4    PT15
#> 61                      Asian Female   PBO   VIS1    PT16
#> 62                      Asian Female   PBO   VIS2    PT16
#> 63                      Asian Female   PBO   VIS3    PT16
#> 64                      Asian Female   PBO   VIS4    PT16
#> 65                      White Female   PBO   VIS1    PT17
#> 66                      White Female   PBO   VIS2    PT17
#> 67                      White Female   PBO   VIS3    PT17
#> 68                      White Female   PBO   VIS4    PT17
#> 69                      Asian   Male   TRT   VIS1    PT18
#> 70                      Asian   Male   TRT   VIS2    PT18
#> 71                      Asian   Male   TRT   VIS3    PT18
#> 72                      Asian   Male   TRT   VIS4    PT18
#> 73                      Asian   Male   TRT   VIS1    PT19
#> 74                      Asian   Male   TRT   VIS2    PT19
#> 75                      Asian   Male   TRT   VIS3    PT19
#> 76                      Asian   Male   TRT   VIS4    PT19
#> 77                      White Female   TRT   VIS1    PT20
#> 78                      White Female   TRT   VIS2    PT20
#> 79                      White Female   TRT   VIS3    PT20
#> 80                      White Female   TRT   VIS4    PT20
#> 81                      White   Male   TRT   VIS1    PT21
#> 82                      White   Male   TRT   VIS2    PT21
#> 83                      White   Male   TRT   VIS3    PT21
#> 84                      White   Male   TRT   VIS4    PT21
#> 85                      White   Male   TRT   VIS1    PT22
#> 86                      White   Male   TRT   VIS2    PT22
#> 87                      White   Male   TRT   VIS3    PT22
#> 88                      White   Male   TRT   VIS4    PT22
#> 89  Black or African American Female   PBO   VIS1    PT23
#> 90  Black or African American Female   PBO   VIS2    PT23
#> 91  Black or African American Female   PBO   VIS3    PT23
#> 92  Black or African American Female   PBO   VIS4    PT23
#> 93                      White Female   TRT   VIS1    PT24
#> 94                      White Female   TRT   VIS2    PT24
#> 95                      White Female   TRT   VIS3    PT24
#> 96                      White Female   TRT   VIS4    PT24
#> 97                      White Female   TRT   VIS1    PT25
#> 98                      White Female   TRT   VIS2    PT25
#> 99                      White Female   TRT   VIS3    PT25
#> 100                     White Female   TRT   VIS4    PT25
#> 101                     White Female   TRT   VIS1    PT26
#> 102                     White Female   TRT   VIS2    PT26
#> 103                     White Female   TRT   VIS3    PT26
#> 104                     White Female   TRT   VIS4    PT26
#> 105 Black or African American Female   TRT   VIS1    PT27
#> 106 Black or African American Female   TRT   VIS2    PT27
#> 107 Black or African American Female   TRT   VIS3    PT27
#> 108 Black or African American Female   TRT   VIS4    PT27
#> 109 Black or African American   Male   TRT   VIS1    PT28
#> 110 Black or African American   Male   TRT   VIS2    PT28
#> 111 Black or African American   Male   TRT   VIS3    PT28
#> 112 Black or African American   Male   TRT   VIS4    PT28
#> 113 Black or African American   Male   TRT   VIS1    PT29
#> 114 Black or African American   Male   TRT   VIS2    PT29
#> 115 Black or African American   Male   TRT   VIS3    PT29
#> 116 Black or African American   Male   TRT   VIS4    PT29
#> 117                     Asian Female   TRT   VIS1    PT30
#> 118                     Asian Female   TRT   VIS2    PT30
#> 119                     Asian Female   TRT   VIS3    PT30
#> 120                     Asian Female   TRT   VIS4    PT30
#> 121                     Asian Female   PBO   VIS1    PT31
#> 122                     Asian Female   PBO   VIS2    PT31
#> 123                     Asian Female   PBO   VIS3    PT31
#> 124                     Asian Female   PBO   VIS4    PT31
#> 125 Black or African American   Male   TRT   VIS1    PT32
#> 126 Black or African American   Male   TRT   VIS2    PT32
#> 127 Black or African American   Male   TRT   VIS3    PT32
#> 128 Black or African American   Male   TRT   VIS4    PT32
#> 129 Black or African American   Male   TRT   VIS1    PT33
#> 130 Black or African American   Male   TRT   VIS2    PT33
#> 131 Black or African American   Male   TRT   VIS3    PT33
#> 132 Black or African American   Male   TRT   VIS4    PT33
#> 133                     Asian Female   PBO   VIS1    PT34
#> 134                     Asian Female   PBO   VIS2    PT34
#> 135                     Asian Female   PBO   VIS3    PT34
#> 136                     Asian Female   PBO   VIS4    PT34
#> 137 Black or African American Female   TRT   VIS1    PT35
#> 138 Black or African American Female   TRT   VIS2    PT35
#> 139 Black or African American Female   TRT   VIS3    PT35
#> 140 Black or African American Female   TRT   VIS4    PT35
#> 141                     Asian Female   TRT   VIS1    PT36
#> 142                     Asian Female   TRT   VIS2    PT36
#> 143                     Asian Female   TRT   VIS3    PT36
#> 144                     Asian Female   TRT   VIS4    PT36
#> 145                     White Female   PBO   VIS1    PT37
#> 146                     White Female   PBO   VIS2    PT37
#> 147                     White Female   PBO   VIS3    PT37
#> 148                     White Female   PBO   VIS4    PT37
#> 149                     Asian Female   PBO   VIS1    PT38
#> 150                     Asian Female   PBO   VIS2    PT38
#> 151                     Asian Female   PBO   VIS3    PT38
#> 152                     Asian Female   PBO   VIS4    PT38
#> 153                     Asian Female   PBO   VIS1    PT39
#> 154                     Asian Female   PBO   VIS2    PT39
#> 155                     Asian Female   PBO   VIS3    PT39
#> 156                     Asian Female   PBO   VIS4    PT39
#> 157 Black or African American   Male   PBO   VIS1    PT40
#> 158 Black or African American   Male   PBO   VIS2    PT40
#> 159 Black or African American   Male   PBO   VIS3    PT40
#> 160 Black or African American   Male   PBO   VIS4    PT40
#> 161 Black or African American   Male   PBO   VIS1    PT41
#> 162 Black or African American   Male   PBO   VIS2    PT41
#> 163 Black or African American   Male   PBO   VIS3    PT41
#> 164 Black or African American   Male   PBO   VIS4    PT41
#> 165 Black or African American   Male   PBO   VIS1    PT42
#> 166 Black or African American   Male   PBO   VIS2    PT42
#> 167 Black or African American   Male   PBO   VIS3    PT42
#> 168 Black or African American   Male   PBO   VIS4    PT42
#> 169 Black or African American Female   TRT   VIS1    PT43
#> 170 Black or African American Female   TRT   VIS2    PT43
#> 171 Black or African American Female   TRT   VIS3    PT43
#> 172 Black or African American Female   TRT   VIS4    PT43
#> 173 Black or African American   Male   TRT   VIS1    PT44
#> 174 Black or African American   Male   TRT   VIS2    PT44
#> 175 Black or African American   Male   TRT   VIS3    PT44
#> 176 Black or African American   Male   TRT   VIS4    PT44
#> 177                     Asian Female   PBO   VIS1    PT45
#> 178                     Asian Female   PBO   VIS2    PT45
#> 179                     Asian Female   PBO   VIS3    PT45
#> 180                     Asian Female   PBO   VIS4    PT45
#> 181                     Asian   Male   PBO   VIS1    PT46
#> 182                     Asian   Male   PBO   VIS2    PT46
#> 183                     Asian   Male   PBO   VIS3    PT46
#> 184                     Asian   Male   PBO   VIS4    PT46
#> 185                     Asian   Male   TRT   VIS1    PT47
#> 186                     Asian   Male   TRT   VIS2    PT47
#> 187                     Asian   Male   TRT   VIS3    PT47
#> 188                     Asian   Male   TRT   VIS4    PT47
#> 189 Black or African American   Male   PBO   VIS1    PT48
#> 190 Black or African American   Male   PBO   VIS2    PT48
#> 191 Black or African American   Male   PBO   VIS3    PT48
#> 192 Black or African American   Male   PBO   VIS4    PT48
#> 193                     White Female   PBO   VIS1    PT49
#> 194                     White Female   PBO   VIS2    PT49
#> 195                     White Female   PBO   VIS3    PT49
#> 196                     White Female   PBO   VIS4    PT49
#> 197                     Asian Female   TRT   VIS1    PT50
#> 198                     Asian Female   TRT   VIS2    PT50
#> 199                     Asian Female   TRT   VIS3    PT50
#> 200                     Asian Female   TRT   VIS4    PT50
#> 201 Black or African American Female   TRT   VIS1    PT51
#> 202 Black or African American Female   TRT   VIS2    PT51
#> 203 Black or African American Female   TRT   VIS3    PT51
#> 204 Black or African American Female   TRT   VIS4    PT51
#> 205 Black or African American Female   TRT   VIS1    PT52
#> 206 Black or African American Female   TRT   VIS2    PT52
#> 207 Black or African American Female   TRT   VIS3    PT52
#> 208 Black or African American Female   TRT   VIS4    PT52
#> 209                     White   Male   PBO   VIS1    PT53
#> 210                     White   Male   PBO   VIS2    PT53
#> 211                     White   Male   PBO   VIS3    PT53
#> 212                     White   Male   PBO   VIS4    PT53
#> 213                     White Female   TRT   VIS1    PT54
#> 214                     White Female   TRT   VIS2    PT54
#> 215                     White Female   TRT   VIS3    PT54
#> 216                     White Female   TRT   VIS4    PT54
#> 217                     Asian   Male   TRT   VIS1    PT55
#> 218                     Asian   Male   TRT   VIS2    PT55
#> 219                     Asian   Male   TRT   VIS3    PT55
#> 220                     Asian   Male   TRT   VIS4    PT55
#> 221 Black or African American   Male   PBO   VIS1    PT56
#> 222 Black or African American   Male   PBO   VIS2    PT56
#> 223 Black or African American   Male   PBO   VIS3    PT56
#> 224 Black or African American   Male   PBO   VIS4    PT56
#> 225 Black or African American   Male   TRT   VIS1    PT57
#> 226 Black or African American   Male   TRT   VIS2    PT57
#> 227 Black or African American   Male   TRT   VIS3    PT57
#> 228 Black or African American   Male   TRT   VIS4    PT57
#> 229                     White Female   TRT   VIS1    PT58
#> 230                     White Female   TRT   VIS2    PT58
#> 231                     White Female   TRT   VIS3    PT58
#> 232                     White Female   TRT   VIS4    PT58
#> 233                     White Female   TRT   VIS1    PT59
#> 234                     White Female   TRT   VIS2    PT59
#> 235                     White Female   TRT   VIS3    PT59
#> 236                     White Female   TRT   VIS4    PT59
#> 237 Black or African American Female   TRT   VIS1    PT60
#> 238 Black or African American Female   TRT   VIS2    PT60
#> 239 Black or African American Female   TRT   VIS3    PT60
#> 240 Black or African American Female   TRT   VIS4    PT60
#> 241                     Asian   Male   TRT   VIS1    PT61
#> 242                     Asian   Male   TRT   VIS2    PT61
#> 243                     Asian   Male   TRT   VIS3    PT61
#> 244                     Asian   Male   TRT   VIS4    PT61
#> 245                     Asian Female   PBO   VIS1    PT62
#> 246                     Asian Female   PBO   VIS2    PT62
#> 247                     Asian Female   PBO   VIS3    PT62
#> 248                     Asian Female   PBO   VIS4    PT62
#> 249                     White Female   PBO   VIS1    PT63
#> 250                     White Female   PBO   VIS2    PT63
#> 251                     White Female   PBO   VIS3    PT63
#> 252                     White Female   PBO   VIS4    PT63
#> 253 Black or African American Female   PBO   VIS1    PT64
#> 254 Black or African American Female   PBO   VIS2    PT64
#> 255 Black or African American Female   PBO   VIS3    PT64
#> 256 Black or African American Female   PBO   VIS4    PT64
#> 257                     Asian Female   TRT   VIS1    PT65
#> 258                     Asian Female   TRT   VIS2    PT65
#> 259                     Asian Female   TRT   VIS3    PT65
#> 260                     Asian Female   TRT   VIS4    PT65
#> 261                     Asian   Male   TRT   VIS1    PT66
#> 262                     Asian   Male   TRT   VIS2    PT66
#> 263                     Asian   Male   TRT   VIS3    PT66
#> 264                     Asian   Male   TRT   VIS4    PT66
#> 265                     White Female   TRT   VIS1    PT67
#> 266                     White Female   TRT   VIS2    PT67
#> 267                     White Female   TRT   VIS3    PT67
#> 268                     White Female   TRT   VIS4    PT67
#> 269                     White Female   TRT   VIS1    PT68
#> 270                     White Female   TRT   VIS2    PT68
#> 271                     White Female   TRT   VIS3    PT68
#> 272                     White Female   TRT   VIS4    PT68
#> 273 Black or African American   Male   PBO   VIS1    PT69
#> 274 Black or African American   Male   PBO   VIS2    PT69
#> 275 Black or African American   Male   PBO   VIS3    PT69
#> 276 Black or African American   Male   PBO   VIS4    PT69
#> 277 Black or African American   Male   PBO   VIS1    PT70
#> 278 Black or African American   Male   PBO   VIS2    PT70
#> 279 Black or African American   Male   PBO   VIS3    PT70
#> 280 Black or African American   Male   PBO   VIS4    PT70
#> 281                     Asian   Male   TRT   VIS1    PT71
#> 282                     Asian   Male   TRT   VIS2    PT71
#> 283                     Asian   Male   TRT   VIS3    PT71
#> 284                     Asian   Male   TRT   VIS4    PT71
#> 285                     Asian Female   TRT   VIS1    PT72
#> 286                     Asian Female   TRT   VIS2    PT72
#> 287                     Asian Female   TRT   VIS3    PT72
#> 288                     Asian Female   TRT   VIS4    PT72
#> 289                     White   Male   PBO   VIS1    PT73
#> 290                     White   Male   PBO   VIS2    PT73
#> 291                     White   Male   PBO   VIS3    PT73
#> 292                     White   Male   PBO   VIS4    PT73
#> 293                     White Female   PBO   VIS1    PT74
#> 294                     White Female   PBO   VIS2    PT74
#> 295                     White Female   PBO   VIS3    PT74
#> 296                     White Female   PBO   VIS4    PT74
#> 297                     White   Male   PBO   VIS1    PT75
#> 298                     White   Male   PBO   VIS2    PT75
#> 299                     White   Male   PBO   VIS3    PT75
#> 300                     White   Male   PBO   VIS4    PT75
#> 301                     White   Male   TRT   VIS1    PT76
#> 302                     White   Male   TRT   VIS2    PT76
#> 303                     White   Male   TRT   VIS3    PT76
#> 304                     White   Male   TRT   VIS4    PT76
#> 305                     Asian   Male   TRT   VIS1    PT77
#> 306                     Asian   Male   TRT   VIS2    PT77
#> 307                     Asian   Male   TRT   VIS3    PT77
#> 308                     Asian   Male   TRT   VIS4    PT77
#> 309                     White Female   TRT   VIS1    PT78
#> 310                     White Female   TRT   VIS2    PT78
#> 311                     White Female   TRT   VIS3    PT78
#> 312                     White Female   TRT   VIS4    PT78
#> 313                     Asian Female   PBO   VIS1    PT79
#> 314                     Asian Female   PBO   VIS2    PT79
#> 315                     Asian Female   PBO   VIS3    PT79
#> 316                     Asian Female   PBO   VIS4    PT79
#> 317                     Asian   Male   TRT   VIS1    PT80
#> 318                     Asian   Male   TRT   VIS2    PT80
#> 319                     Asian   Male   TRT   VIS3    PT80
#> 320                     Asian   Male   TRT   VIS4    PT80
#> 321                     White   Male   PBO   VIS1    PT81
#> 322                     White   Male   PBO   VIS2    PT81
#> 323                     White   Male   PBO   VIS3    PT81
#> 324                     White   Male   PBO   VIS4    PT81
#> 325 Black or African American Female   PBO   VIS1    PT82
#> 326 Black or African American Female   PBO   VIS2    PT82
#> 327 Black or African American Female   PBO   VIS3    PT82
#> 328 Black or African American Female   PBO   VIS4    PT82
#> 329                     White   Male   TRT   VIS1    PT83
#> 330                     White   Male   TRT   VIS2    PT83
#> 331                     White   Male   TRT   VIS3    PT83
#> 332                     White   Male   TRT   VIS4    PT83
#> 333                     Asian Female   PBO   VIS1    PT84
#> 334                     Asian Female   PBO   VIS2    PT84
#> 335                     Asian Female   PBO   VIS3    PT84
#> 336                     Asian Female   PBO   VIS4    PT84
#> 337                     Asian Female   PBO   VIS1    PT85
#> 338                     Asian Female   PBO   VIS2    PT85
#> 339                     Asian Female   PBO   VIS3    PT85
#> 340                     Asian Female   PBO   VIS4    PT85
#> 341 Black or African American Female   TRT   VIS1    PT86
#> 342 Black or African American Female   TRT   VIS2    PT86
#> 343 Black or African American Female   TRT   VIS3    PT86
#> 344 Black or African American Female   TRT   VIS4    PT86
#> 345                     White   Male   PBO   VIS1    PT87
#> 346                     White   Male   PBO   VIS2    PT87
#> 347                     White   Male   PBO   VIS3    PT87
#> 348                     White   Male   PBO   VIS4    PT87
#> 349 Black or African American Female   PBO   VIS1    PT88
#> 350 Black or African American Female   PBO   VIS2    PT88
#> 351 Black or African American Female   PBO   VIS3    PT88
#> 352 Black or African American Female   PBO   VIS4    PT88
#> 353 Black or African American   Male   PBO   VIS1    PT89
#> 354 Black or African American   Male   PBO   VIS2    PT89
#> 355 Black or African American   Male   PBO   VIS3    PT89
#> 356 Black or African American   Male   PBO   VIS4    PT89
#> 357                     Asian   Male   PBO   VIS1    PT90
#> 358                     Asian   Male   PBO   VIS2    PT90
#> 359                     Asian   Male   PBO   VIS3    PT90
#> 360                     Asian   Male   PBO   VIS4    PT90
#> 361                     White Female   TRT   VIS1    PT91
#> 362                     White Female   TRT   VIS2    PT91
#> 363                     White Female   TRT   VIS3    PT91
#> 364                     White Female   TRT   VIS4    PT91
#> 365                     White Female   TRT   VIS1    PT92
#> 366                     White Female   TRT   VIS2    PT92
#> 367                     White Female   TRT   VIS3    PT92
#> 368                     White Female   TRT   VIS4    PT92
#> 369 Black or African American   Male   PBO   VIS1    PT93
#> 370 Black or African American   Male   PBO   VIS2    PT93
#> 371 Black or African American   Male   PBO   VIS3    PT93
#> 372 Black or African American   Male   PBO   VIS4    PT93
#> 373                     Asian Female   PBO   VIS1    PT94
#> 374                     Asian Female   PBO   VIS2    PT94
#> 375                     Asian Female   PBO   VIS3    PT94
#> 376                     Asian Female   PBO   VIS4    PT94
#> 377                     Asian Female   PBO   VIS1    PT95
#> 378                     Asian Female   PBO   VIS2    PT95
#> 379                     Asian Female   PBO   VIS3    PT95
#> 380                     Asian Female   PBO   VIS4    PT95
#> 381 Black or African American Female   PBO   VIS1    PT96
#> 382 Black or African American Female   PBO   VIS2    PT96
#> 383 Black or African American Female   PBO   VIS3    PT96
#> 384 Black or African American Female   PBO   VIS4    PT96
#> 385                     White   Male   TRT   VIS1    PT97
#> 386                     White   Male   TRT   VIS2    PT97
#> 387                     White   Male   TRT   VIS3    PT97
#> 388                     White   Male   TRT   VIS4    PT97
#> 389                     Asian   Male   PBO   VIS1    PT98
#> 390                     Asian   Male   PBO   VIS2    PT98
#> 391                     Asian   Male   PBO   VIS3    PT98
#> 392                     Asian   Male   PBO   VIS4    PT98
#> 393 Black or African American   Male   PBO   VIS1    PT99
#> 394 Black or African American   Male   PBO   VIS2    PT99
#> 395 Black or African American   Male   PBO   VIS3    PT99
#> 396 Black or African American   Male   PBO   VIS4    PT99
#> 397 Black or African American Female   PBO   VIS1   PT100
#> 398 Black or African American Female   PBO   VIS2   PT100
#> 399 Black or African American Female   PBO   VIS3   PT100
#> 400 Black or African American Female   PBO   VIS4   PT100
#> 401                     White   Male   TRT   VIS1   PT101
#> 402                     White   Male   TRT   VIS2   PT101
#> 403                     White   Male   TRT   VIS3   PT101
#> 404                     White   Male   TRT   VIS4   PT101
#> 405                     Asian Female   PBO   VIS1   PT102
#> 406                     Asian Female   PBO   VIS2   PT102
#> 407                     Asian Female   PBO   VIS3   PT102
#> 408                     Asian Female   PBO   VIS4   PT102
#> 409                     Asian Female   PBO   VIS1   PT103
#> 410                     Asian Female   PBO   VIS2   PT103
#> 411                     Asian Female   PBO   VIS3   PT103
#> 412                     Asian Female   PBO   VIS4   PT103
#> 413 Black or African American Female   PBO   VIS1   PT104
#> 414 Black or African American Female   PBO   VIS2   PT104
#> 415 Black or African American Female   PBO   VIS3   PT104
#> 416 Black or African American Female   PBO   VIS4   PT104
#> 417 Black or African American Female   TRT   VIS1   PT105
#> 418 Black or African American Female   TRT   VIS2   PT105
#> 419 Black or African American Female   TRT   VIS3   PT105
#> 420 Black or African American Female   TRT   VIS4   PT105
#> 421                     Asian Female   TRT   VIS1   PT106
#> 422                     Asian Female   TRT   VIS2   PT106
#> 423                     Asian Female   TRT   VIS3   PT106
#> 424                     Asian Female   TRT   VIS4   PT106
#> 425                     White Female   PBO   VIS1   PT107
#> 426                     White Female   PBO   VIS2   PT107
#> 427                     White Female   PBO   VIS3   PT107
#> 428                     White Female   PBO   VIS4   PT107
#> 429                     White   Male   TRT   VIS1   PT108
#> 430                     White   Male   TRT   VIS2   PT108
#> 431                     White   Male   TRT   VIS3   PT108
#> 432                     White   Male   TRT   VIS4   PT108
#> 433 Black or African American Female   PBO   VIS1   PT109
#> 434 Black or African American Female   PBO   VIS2   PT109
#> 435 Black or African American Female   PBO   VIS3   PT109
#> 436 Black or African American Female   PBO   VIS4   PT109
#> 437                     Asian   Male   TRT   VIS1   PT110
#> 438                     Asian   Male   TRT   VIS2   PT110
#> 439                     Asian   Male   TRT   VIS3   PT110
#> 440                     Asian   Male   TRT   VIS4   PT110
#> 441                     White Female   PBO   VIS1   PT111
#> 442                     White Female   PBO   VIS2   PT111
#> 443                     White Female   PBO   VIS3   PT111
#> 444                     White Female   PBO   VIS4   PT111
#> 445 Black or African American Female   PBO   VIS1   PT112
#> 446 Black or African American Female   PBO   VIS2   PT112
#> 447 Black or African American Female   PBO   VIS3   PT112
#> 448 Black or African American Female   PBO   VIS4   PT112
#> 449                     Asian   Male   PBO   VIS1   PT113
#> 450                     Asian   Male   PBO   VIS2   PT113
#> 451                     Asian   Male   PBO   VIS3   PT113
#> 452                     Asian   Male   PBO   VIS4   PT113
#> 453 Black or African American   Male   PBO   VIS1   PT114
#> 454 Black or African American   Male   PBO   VIS2   PT114
#> 455 Black or African American   Male   PBO   VIS3   PT114
#> 456 Black or African American   Male   PBO   VIS4   PT114
#> 457                     Asian Female   PBO   VIS1   PT115
#> 458                     Asian Female   PBO   VIS2   PT115
#> 459                     Asian Female   PBO   VIS3   PT115
#> 460                     Asian Female   PBO   VIS4   PT115
#> 461 Black or African American   Male   TRT   VIS1   PT116
#> 462 Black or African American   Male   TRT   VIS2   PT116
#> 463 Black or African American   Male   TRT   VIS3   PT116
#> 464 Black or African American   Male   TRT   VIS4   PT116
#> 465 Black or African American   Male   PBO   VIS1   PT117
#> 466 Black or African American   Male   PBO   VIS2   PT117
#> 467 Black or African American   Male   PBO   VIS3   PT117
#> 468 Black or African American   Male   PBO   VIS4   PT117
#> 469                     Asian Female   PBO   VIS1   PT118
#> 470                     Asian Female   PBO   VIS2   PT118
#> 471                     Asian Female   PBO   VIS3   PT118
#> 472                     Asian Female   PBO   VIS4   PT118
#> 473                     White   Male   TRT   VIS1   PT119
#> 474                     White   Male   TRT   VIS2   PT119
#> 475                     White   Male   TRT   VIS3   PT119
#> 476                     White   Male   TRT   VIS4   PT119
#> 477                     Asian Female   PBO   VIS1   PT120
#> 478                     Asian Female   PBO   VIS2   PT120
#> 479                     Asian Female   PBO   VIS3   PT120
#> 480                     Asian Female   PBO   VIS4   PT120
#> 481 Black or African American   Male   TRT   VIS1   PT121
#> 482 Black or African American   Male   TRT   VIS2   PT121
#> 483 Black or African American   Male   TRT   VIS3   PT121
#> 484 Black or African American   Male   TRT   VIS4   PT121
#> 485                     Asian Female   PBO   VIS1   PT122
#> 486                     Asian Female   PBO   VIS2   PT122
#> 487                     Asian Female   PBO   VIS3   PT122
#> 488                     Asian Female   PBO   VIS4   PT122
#> 489                     White Female   PBO   VIS1   PT123
#> 490                     White Female   PBO   VIS2   PT123
#> 491                     White Female   PBO   VIS3   PT123
#> 492                     White Female   PBO   VIS4   PT123
#> 493                     Asian Female   TRT   VIS1   PT124
#> 494                     Asian Female   TRT   VIS2   PT124
#> 495                     Asian Female   TRT   VIS3   PT124
#> 496                     Asian Female   TRT   VIS4   PT124
#> 497                     White   Male   PBO   VIS1   PT125
#> 498                     White   Male   PBO   VIS2   PT125
#> 499                     White   Male   PBO   VIS3   PT125
#> 500                     White   Male   PBO   VIS4   PT125
#> 501                     Asian   Male   TRT   VIS1   PT126
#> 502                     Asian   Male   TRT   VIS2   PT126
#> 503                     Asian   Male   TRT   VIS3   PT126
#> 504                     Asian   Male   TRT   VIS4   PT126
#> 505 Black or African American Female   TRT   VIS1   PT127
#> 506 Black or African American Female   TRT   VIS2   PT127
#> 507 Black or African American Female   TRT   VIS3   PT127
#> 508 Black or African American Female   TRT   VIS4   PT127
#> 509                     Asian Female   TRT   VIS1   PT128
#> 510                     Asian Female   TRT   VIS2   PT128
#> 511                     Asian Female   TRT   VIS3   PT128
#> 512                     Asian Female   TRT   VIS4   PT128
#> 513                     White   Male   TRT   VIS1   PT129
#> 514                     White   Male   TRT   VIS2   PT129
#> 515                     White   Male   TRT   VIS3   PT129
#> 516                     White   Male   TRT   VIS4   PT129
#> 517                     White   Male   PBO   VIS1   PT130
#> 518                     White   Male   PBO   VIS2   PT130
#> 519                     White   Male   PBO   VIS3   PT130
#> 520                     White   Male   PBO   VIS4   PT130
#> 521                     Asian Female   PBO   VIS1   PT131
#> 522                     Asian Female   PBO   VIS2   PT131
#> 523                     Asian Female   PBO   VIS3   PT131
#> 524                     Asian Female   PBO   VIS4   PT131
#> 525                     Asian   Male   PBO   VIS1   PT132
#> 526                     Asian   Male   PBO   VIS2   PT132
#> 527                     Asian   Male   PBO   VIS3   PT132
#> 528                     Asian   Male   PBO   VIS4   PT132
#> 529                     White   Male   PBO   VIS1   PT133
#> 530                     White   Male   PBO   VIS2   PT133
#> 531                     White   Male   PBO   VIS3   PT133
#> 532                     White   Male   PBO   VIS4   PT133
#> 533 Black or African American   Male   PBO   VIS1   PT134
#> 534 Black or African American   Male   PBO   VIS2   PT134
#> 535 Black or African American   Male   PBO   VIS3   PT134
#> 536 Black or African American   Male   PBO   VIS4   PT134
#> 537 Black or African American   Male   PBO   VIS1   PT135
#> 538 Black or African American   Male   PBO   VIS2   PT135
#> 539 Black or African American   Male   PBO   VIS3   PT135
#> 540 Black or African American   Male   PBO   VIS4   PT135
#> 541                     White Female   TRT   VIS1   PT136
#> 542                     White Female   TRT   VIS2   PT136
#> 543                     White Female   TRT   VIS3   PT136
#> 544                     White Female   TRT   VIS4   PT136
#> 545                     Asian Female   TRT   VIS1   PT137
#> 546                     Asian Female   TRT   VIS2   PT137
#> 547                     Asian Female   TRT   VIS3   PT137
#> 548                     Asian Female   TRT   VIS4   PT137
#> 549                     Asian   Male   TRT   VIS1   PT138
#> 550                     Asian   Male   TRT   VIS2   PT138
#> 551                     Asian   Male   TRT   VIS3   PT138
#> 552                     Asian   Male   TRT   VIS4   PT138
#> 553                     Asian Female   TRT   VIS1   PT139
#> 554                     Asian Female   TRT   VIS2   PT139
#> 555                     Asian Female   TRT   VIS3   PT139
#> 556                     Asian Female   TRT   VIS4   PT139
#> 557 Black or African American Female   PBO   VIS1   PT140
#> 558 Black or African American Female   PBO   VIS2   PT140
#> 559 Black or African American Female   PBO   VIS3   PT140
#> 560 Black or African American Female   PBO   VIS4   PT140
#> 561                     White   Male   TRT   VIS1   PT141
#> 562                     White   Male   TRT   VIS2   PT141
#> 563                     White   Male   TRT   VIS3   PT141
#> 564                     White   Male   TRT   VIS4   PT141
#> 565 Black or African American   Male   TRT   VIS1   PT142
#> 566 Black or African American   Male   TRT   VIS2   PT142
#> 567 Black or African American   Male   TRT   VIS3   PT142
#> 568 Black or African American   Male   TRT   VIS4   PT142
#> 569 Black or African American Female   TRT   VIS1   PT143
#> 570 Black or African American Female   TRT   VIS2   PT143
#> 571 Black or African American Female   TRT   VIS3   PT143
#> 572 Black or African American Female   TRT   VIS4   PT143
#> 573                     Asian Female   PBO   VIS1   PT144
#> 574                     Asian Female   PBO   VIS2   PT144
#> 575                     Asian Female   PBO   VIS3   PT144
#> 576                     Asian Female   PBO   VIS4   PT144
#> 577 Black or African American   Male   PBO   VIS1   PT145
#> 578 Black or African American   Male   PBO   VIS2   PT145
#> 579 Black or African American   Male   PBO   VIS3   PT145
#> 580 Black or African American   Male   PBO   VIS4   PT145
#> 581                     Asian Female   TRT   VIS1   PT146
#> 582                     Asian Female   TRT   VIS2   PT146
#> 583                     Asian Female   TRT   VIS3   PT146
#> 584                     Asian Female   TRT   VIS4   PT146
#> 585                     Asian Female   PBO   VIS1   PT147
#> 586                     Asian Female   PBO   VIS2   PT147
#> 587                     Asian Female   PBO   VIS3   PT147
#> 588                     Asian Female   PBO   VIS4   PT147
#> 589 Black or African American   Male   TRT   VIS1   PT148
#> 590 Black or African American   Male   TRT   VIS2   PT148
#> 591 Black or African American   Male   TRT   VIS3   PT148
#> 592 Black or African American   Male   TRT   VIS4   PT148
#> 593                     Asian Female   PBO   VIS1   PT149
#> 594                     Asian Female   PBO   VIS2   PT149
#> 595                     Asian Female   PBO   VIS3   PT149
#> 596                     Asian Female   PBO   VIS4   PT149
#> 597                     White   Male   PBO   VIS1   PT150
#> 598                     White   Male   PBO   VIS2   PT150
#> 599                     White   Male   PBO   VIS3   PT150
#> 600                     White   Male   PBO   VIS4   PT150
#> 601                     Asian Female   PBO   VIS1   PT151
#> 602                     Asian Female   PBO   VIS2   PT151
#> 603                     Asian Female   PBO   VIS3   PT151
#> 604                     Asian Female   PBO   VIS4   PT151
#> 605                     Asian Female   TRT   VIS1   PT152
#> 606                     Asian Female   TRT   VIS2   PT152
#> 607                     Asian Female   TRT   VIS3   PT152
#> 608                     Asian Female   TRT   VIS4   PT152
#> 609 Black or African American   Male   PBO   VIS1   PT153
#> 610 Black or African American   Male   PBO   VIS2   PT153
#> 611 Black or African American   Male   PBO   VIS3   PT153
#> 612 Black or African American   Male   PBO   VIS4   PT153
#> 613 Black or African American Female   PBO   VIS1   PT154
#> 614 Black or African American Female   PBO   VIS2   PT154
#> 615 Black or African American Female   PBO   VIS3   PT154
#> 616 Black or African American Female   PBO   VIS4   PT154
#> 617                     Asian   Male   TRT   VIS1   PT155
#> 618                     Asian   Male   TRT   VIS2   PT155
#> 619                     Asian   Male   TRT   VIS3   PT155
#> 620                     Asian   Male   TRT   VIS4   PT155
#> 621                     White   Male   PBO   VIS1   PT156
#> 622                     White   Male   PBO   VIS2   PT156
#> 623                     White   Male   PBO   VIS3   PT156
#> 624                     White   Male   PBO   VIS4   PT156
#> 625 Black or African American   Male   TRT   VIS1   PT157
#> 626 Black or African American   Male   TRT   VIS2   PT157
#> 627 Black or African American   Male   TRT   VIS3   PT157
#> 628 Black or African American   Male   TRT   VIS4   PT157
#> 629 Black or African American   Male   TRT   VIS1   PT158
#> 630 Black or African American   Male   TRT   VIS2   PT158
#> 631 Black or African American   Male   TRT   VIS3   PT158
#> 632 Black or African American   Male   TRT   VIS4   PT158
#> 633                     Asian Female   TRT   VIS1   PT159
#> 634                     Asian Female   TRT   VIS2   PT159
#> 635                     Asian Female   TRT   VIS3   PT159
#> 636                     Asian Female   TRT   VIS4   PT159
#> 637                     Asian   Male   TRT   VIS1   PT160
#> 638                     Asian   Male   TRT   VIS2   PT160
#> 639                     Asian   Male   TRT   VIS3   PT160
#> 640                     Asian   Male   TRT   VIS4   PT160
#> 641 Black or African American Female   PBO   VIS1   PT161
#> 642 Black or African American Female   PBO   VIS2   PT161
#> 643 Black or African American Female   PBO   VIS3   PT161
#> 644 Black or African American Female   PBO   VIS4   PT161
#> 645                     Asian   Male   PBO   VIS1   PT162
#> 646                     Asian   Male   PBO   VIS2   PT162
#> 647                     Asian   Male   PBO   VIS3   PT162
#> 648                     Asian   Male   PBO   VIS4   PT162
#> 649                     White   Male   PBO   VIS1   PT163
#> 650                     White   Male   PBO   VIS2   PT163
#> 651                     White   Male   PBO   VIS3   PT163
#> 652                     White   Male   PBO   VIS4   PT163
#> 653 Black or African American Female   PBO   VIS1   PT164
#> 654 Black or African American Female   PBO   VIS2   PT164
#> 655 Black or African American Female   PBO   VIS3   PT164
#> 656 Black or African American Female   PBO   VIS4   PT164
#> 657                     White Female   TRT   VIS1   PT165
#> 658                     White Female   TRT   VIS2   PT165
#> 659                     White Female   TRT   VIS3   PT165
#> 660                     White Female   TRT   VIS4   PT165
#> 661                     Asian   Male   TRT   VIS1   PT166
#> 662                     Asian   Male   TRT   VIS2   PT166
#> 663                     Asian   Male   TRT   VIS3   PT166
#> 664                     Asian   Male   TRT   VIS4   PT166
#> 665 Black or African American   Male   PBO   VIS1   PT167
#> 666 Black or African American   Male   PBO   VIS2   PT167
#> 667 Black or African American   Male   PBO   VIS3   PT167
#> 668 Black or African American   Male   PBO   VIS4   PT167
#> 669 Black or African American Female   PBO   VIS1   PT168
#> 670 Black or African American Female   PBO   VIS2   PT168
#> 671 Black or African American Female   PBO   VIS3   PT168
#> 672 Black or African American Female   PBO   VIS4   PT168
#> 673                     White Female   PBO   VIS1   PT169
#> 674                     White Female   PBO   VIS2   PT169
#> 675                     White Female   PBO   VIS3   PT169
#> 676                     White Female   PBO   VIS4   PT169
#> 677                     White Female   TRT   VIS1   PT170
#> 678                     White Female   TRT   VIS2   PT170
#> 679                     White Female   TRT   VIS3   PT170
#> 680                     White Female   TRT   VIS4   PT170
#> 681                     Asian Female   PBO   VIS1   PT171
#> 682                     Asian Female   PBO   VIS2   PT171
#> 683                     Asian Female   PBO   VIS3   PT171
#> 684                     Asian Female   PBO   VIS4   PT171
#> 685                     Asian Female   TRT   VIS1   PT172
#> 686                     Asian Female   TRT   VIS2   PT172
#> 687                     Asian Female   TRT   VIS3   PT172
#> 688                     Asian Female   TRT   VIS4   PT172
#> 689                     White Female   TRT   VIS1   PT173
#> 690                     White Female   TRT   VIS2   PT173
#> 691                     White Female   TRT   VIS3   PT173
#> 692                     White Female   TRT   VIS4   PT173
#> 693 Black or African American Female   TRT   VIS1   PT174
#> 694 Black or African American Female   TRT   VIS2   PT174
#> 695 Black or African American Female   TRT   VIS3   PT174
#> 696 Black or African American Female   TRT   VIS4   PT174
#> 697 Black or African American Female   PBO   VIS1   PT175
#> 698 Black or African American Female   PBO   VIS2   PT175
#> 699 Black or African American Female   PBO   VIS3   PT175
#> 700 Black or African American Female   PBO   VIS4   PT175
#> 701 Black or African American   Male   TRT   VIS1   PT176
#> 702 Black or African American   Male   TRT   VIS2   PT176
#> 703 Black or African American   Male   TRT   VIS3   PT176
#> 704 Black or African American   Male   TRT   VIS4   PT176
#> 705 Black or African American   Male   PBO   VIS1   PT177
#> 706 Black or African American   Male   PBO   VIS2   PT177
#> 707 Black or African American   Male   PBO   VIS3   PT177
#> 708 Black or African American   Male   PBO   VIS4   PT177
#> 709 Black or African American   Male   PBO   VIS1   PT178
#> 710 Black or African American   Male   PBO   VIS2   PT178
#> 711 Black or African American   Male   PBO   VIS3   PT178
#> 712 Black or African American   Male   PBO   VIS4   PT178
#> 713                     White   Male   TRT   VIS1   PT179
#> 714                     White   Male   TRT   VIS2   PT179
#> 715                     White   Male   TRT   VIS3   PT179
#> 716                     White   Male   TRT   VIS4   PT179
#> 717                     Asian   Male   PBO   VIS1   PT180
#> 718                     Asian   Male   PBO   VIS2   PT180
#> 719                     Asian   Male   PBO   VIS3   PT180
#> 720                     Asian   Male   PBO   VIS4   PT180
#> 721                     Asian Female   PBO   VIS1   PT181
#> 722                     Asian Female   PBO   VIS2   PT181
#> 723                     Asian Female   PBO   VIS3   PT181
#> 724                     Asian Female   PBO   VIS4   PT181
#> 725 Black or African American Female   TRT   VIS1   PT182
#> 726 Black or African American Female   TRT   VIS2   PT182
#> 727 Black or African American Female   TRT   VIS3   PT182
#> 728 Black or African American Female   TRT   VIS4   PT182
#> 729 Black or African American Female   TRT   VIS1   PT183
#> 730 Black or African American Female   TRT   VIS2   PT183
#> 731 Black or African American Female   TRT   VIS3   PT183
#> 732 Black or African American Female   TRT   VIS4   PT183
#> 733                     White Female   TRT   VIS1   PT184
#> 734                     White Female   TRT   VIS2   PT184
#> 735                     White Female   TRT   VIS3   PT184
#> 736                     White Female   TRT   VIS4   PT184
#> 737 Black or African American Female   PBO   VIS1   PT185
#> 738 Black or African American Female   PBO   VIS2   PT185
#> 739 Black or African American Female   PBO   VIS3   PT185
#> 740 Black or African American Female   PBO   VIS4   PT185
#> 741                     White   Male   PBO   VIS1   PT186
#> 742                     White   Male   PBO   VIS2   PT186
#> 743                     White   Male   PBO   VIS3   PT186
#> 744                     White   Male   PBO   VIS4   PT186
#> 745 Black or African American Female   PBO   VIS1   PT187
#> 746 Black or African American Female   PBO   VIS2   PT187
#> 747 Black or African American Female   PBO   VIS3   PT187
#> 748 Black or African American Female   PBO   VIS4   PT187
#> 749 Black or African American Female   PBO   VIS1   PT188
#> 750 Black or African American Female   PBO   VIS2   PT188
#> 751 Black or African American Female   PBO   VIS3   PT188
#> 752 Black or African American Female   PBO   VIS4   PT188
#> 753                     Asian Female   TRT   VIS1   PT189
#> 754                     Asian Female   TRT   VIS2   PT189
#> 755                     Asian Female   TRT   VIS3   PT189
#> 756                     Asian Female   TRT   VIS4   PT189
#> 757 Black or African American Female   TRT   VIS1   PT190
#> 758 Black or African American Female   TRT   VIS2   PT190
#> 759 Black or African American Female   TRT   VIS3   PT190
#> 760 Black or African American Female   TRT   VIS4   PT190
#> 761                     White   Male   TRT   VIS1   PT191
#> 762                     White   Male   TRT   VIS2   PT191
#> 763                     White   Male   TRT   VIS3   PT191
#> 764                     White   Male   TRT   VIS4   PT191
#> 765                     White Female   TRT   VIS1   PT192
#> 766                     White Female   TRT   VIS2   PT192
#> 767                     White Female   TRT   VIS3   PT192
#> 768                     White Female   TRT   VIS4   PT192
#> 769                     Asian   Male   PBO   VIS1   PT193
#> 770                     Asian   Male   PBO   VIS2   PT193
#> 771                     Asian   Male   PBO   VIS3   PT193
#> 772                     Asian   Male   PBO   VIS4   PT193
#> 773 Black or African American   Male   PBO   VIS1   PT194
#> 774 Black or African American   Male   PBO   VIS2   PT194
#> 775 Black or African American   Male   PBO   VIS3   PT194
#> 776 Black or African American   Male   PBO   VIS4   PT194
#> 777                     White Female   TRT   VIS1   PT195
#> 778                     White Female   TRT   VIS2   PT195
#> 779                     White Female   TRT   VIS3   PT195
#> 780                     White Female   TRT   VIS4   PT195
#> 781                     Asian   Male   PBO   VIS1   PT196
#> 782                     Asian   Male   PBO   VIS2   PT196
#> 783                     Asian   Male   PBO   VIS3   PT196
#> 784                     Asian   Male   PBO   VIS4   PT196
#> 785 Black or African American   Male   PBO   VIS1   PT197
#> 786 Black or African American   Male   PBO   VIS2   PT197
#> 787 Black or African American   Male   PBO   VIS3   PT197
#> 788 Black or African American   Male   PBO   VIS4   PT197
#> 789 Black or African American   Male   TRT   VIS1   PT198
#> 790 Black or African American   Male   TRT   VIS2   PT198
#> 791 Black or African American   Male   TRT   VIS3   PT198
#> 792 Black or African American   Male   TRT   VIS4   PT198
#> 793                     White   Male   TRT   VIS1   PT199
#> 794                     White   Male   TRT   VIS2   PT199
#> 795                     White   Male   TRT   VIS3   PT199
#> 796                     White   Male   TRT   VIS4   PT199
#> 797 Black or African American   Male   PBO   VIS1   PT200
#> 798 Black or African American   Male   PBO   VIS2   PT200
#> 799 Black or African American   Male   PBO   VIS3   PT200
#> 800 Black or African American   Male   PBO   VIS4   PT200
# Model matrix:
model.matrix(object)
#>     (Intercept) RACEBlack or African American RACEWhite SEXFemale ARMCDTRT
#> 2             1                             1         0         1        1
#> 4             1                             1         0         1        1
#> 6             1                             0         0         0        0
#> 7             1                             0         0         0        0
#> 8             1                             0         0         0        0
#> 10            1                             1         0         1        0
#> 12            1                             1         0         1        0
#> 13            1                             0         0         1        1
#> 14            1                             0         0         1        1
#> 16            1                             0         0         1        1
#> 17            1                             1         0         0        0
#> 19            1                             1         0         0        0
#> 20            1                             1         0         0        0
#> 23            1                             1         0         0        0
#> 25            1                             0         0         1        0
#> 26            1                             0         0         1        0
#> 28            1                             0         0         1        0
#> 29            1                             1         0         0        0
#> 30            1                             1         0         0        0
#> 31            1                             1         0         0        0
#> 32            1                             1         0         0        0
#> 33            1                             0         1         0        1
#> 34            1                             0         1         0        1
#> 36            1                             0         1         0        1
#> 39            1                             1         0         1        0
#> 41            1                             0         0         1        1
#> 42            1                             0         0         1        1
#> 43            1                             0         0         1        1
#> 44            1                             0         0         1        1
#> 45            1                             0         0         0        0
#> 46            1                             0         0         0        0
#> 47            1                             0         0         0        0
#> 51            1                             0         1         0        1
#> 52            1                             0         1         0        1
#> 55            1                             1         0         0        0
#> 59            1                             0         0         0        0
#> 60            1                             0         0         0        0
#> 62            1                             0         0         1        0
#> 64            1                             0         0         1        0
#> 65            1                             0         1         1        0
#> 68            1                             0         1         1        0
#> 69            1                             0         0         0        1
#> 70            1                             0         0         0        1
#> 72            1                             0         0         0        1
#> 73            1                             0         0         0        1
#> 74            1                             0         0         0        1
#> 75            1                             0         0         0        1
#> 76            1                             0         0         0        1
#> 78            1                             0         1         1        1
#> 79            1                             0         1         1        1
#> 82            1                             0         1         0        1
#> 83            1                             0         1         0        1
#> 84            1                             0         1         0        1
#> 85            1                             0         1         0        1
#> 86            1                             0         1         0        1
#> 87            1                             0         1         0        1
#> 88            1                             0         1         0        1
#> 89            1                             1         0         1        0
#> 90            1                             1         0         1        0
#> 91            1                             1         0         1        0
#> 93            1                             0         1         1        1
#> 94            1                             0         1         1        1
#> 95            1                             0         1         1        1
#> 96            1                             0         1         1        1
#> 97            1                             0         1         1        1
#> 98            1                             0         1         1        1
#> 99            1                             0         1         1        1
#> 100           1                             0         1         1        1
#> 101           1                             0         1         1        1
#> 102           1                             0         1         1        1
#> 103           1                             0         1         1        1
#> 104           1                             0         1         1        1
#> 105           1                             1         0         1        1
#> 107           1                             1         0         1        1
#> 108           1                             1         0         1        1
#> 109           1                             1         0         0        1
#> 110           1                             1         0         0        1
#> 111           1                             1         0         0        1
#> 112           1                             1         0         0        1
#> 113           1                             1         0         0        1
#> 114           1                             1         0         0        1
#> 116           1                             1         0         0        1
#> 117           1                             0         0         1        1
#> 118           1                             0         0         1        1
#> 119           1                             0         0         1        1
#> 120           1                             0         0         1        1
#> 121           1                             0         0         1        0
#> 123           1                             0         0         1        0
#> 125           1                             1         0         0        1
#> 128           1                             1         0         0        1
#> 129           1                             1         0         0        1
#> 130           1                             1         0         0        1
#> 132           1                             1         0         0        1
#> 133           1                             0         0         1        0
#> 134           1                             0         0         1        0
#> 135           1                             0         0         1        0
#> 136           1                             0         0         1        0
#> 137           1                             1         0         1        1
#> 138           1                             1         0         1        1
#> 140           1                             1         0         1        1
#> 142           1                             0         0         1        1
#> 144           1                             0         0         1        1
#> 145           1                             0         1         1        0
#> 146           1                             0         1         1        0
#> 147           1                             0         1         1        0
#> 148           1                             0         1         1        0
#> 149           1                             0         0         1        0
#> 151           1                             0         0         1        0
#> 153           1                             0         0         1        0
#> 155           1                             0         0         1        0
#> 156           1                             0         0         1        0
#> 157           1                             1         0         0        0
#> 158           1                             1         0         0        0
#> 159           1                             1         0         0        0
#> 162           1                             1         0         0        0
#> 163           1                             1         0         0        0
#> 164           1                             1         0         0        0
#> 165           1                             1         0         0        0
#> 168           1                             1         0         0        0
#> 169           1                             1         0         1        1
#> 170           1                             1         0         1        1
#> 171           1                             1         0         1        1
#> 172           1                             1         0         1        1
#> 173           1                             1         0         0        1
#> 177           1                             0         0         1        0
#> 178           1                             0         0         1        0
#> 179           1                             0         0         1        0
#> 180           1                             0         0         1        0
#> 181           1                             0         0         0        0
#> 182           1                             0         0         0        0
#> 183           1                             0         0         0        0
#> 185           1                             0         0         0        1
#> 186           1                             0         0         0        1
#> 187           1                             0         0         0        1
#> 190           1                             1         0         0        0
#> 191           1                             1         0         0        0
#> 193           1                             0         1         1        0
#> 194           1                             0         1         1        0
#> 195           1                             0         1         1        0
#> 197           1                             0         0         1        1
#> 198           1                             0         0         1        1
#> 199           1                             0         0         1        1
#> 201           1                             1         0         1        1
#> 202           1                             1         0         1        1
#> 204           1                             1         0         1        1
#> 206           1                             1         0         1        1
#> 208           1                             1         0         1        1
#> 209           1                             0         1         0        0
#> 210           1                             0         1         0        0
#> 217           1                             0         0         0        1
#> 218           1                             0         0         0        1
#> 219           1                             0         0         0        1
#> 221           1                             1         0         0        0
#> 224           1                             1         0         0        0
#> 226           1                             1         0         0        1
#> 227           1                             1         0         0        1
#> 228           1                             1         0         0        1
#> 230           1                             0         1         1        1
#> 231           1                             0         1         1        1
#> 233           1                             0         1         1        1
#> 235           1                             0         1         1        1
#> 236           1                             0         1         1        1
#> 237           1                             1         0         1        1
#> 238           1                             1         0         1        1
#> 239           1                             1         0         1        1
#> 240           1                             1         0         1        1
#> 241           1                             0         0         0        1
#> 242           1                             0         0         0        1
#> 244           1                             0         0         0        1
#> 246           1                             0         0         1        0
#> 250           1                             0         1         1        0
#> 251           1                             0         1         1        0
#> 252           1                             0         1         1        0
#> 253           1                             1         0         1        0
#> 254           1                             1         0         1        0
#> 256           1                             1         0         1        0
#> 257           1                             0         0         1        1
#> 258           1                             0         0         1        1
#> 259           1                             0         0         1        1
#> 260           1                             0         0         1        1
#> 261           1                             0         0         0        1
#> 262           1                             0         0         0        1
#> 263           1                             0         0         0        1
#> 264           1                             0         0         0        1
#> 265           1                             0         1         1        1
#> 266           1                             0         1         1        1
#> 267           1                             0         1         1        1
#> 268           1                             0         1         1        1
#> 269           1                             0         1         1        1
#> 270           1                             0         1         1        1
#> 273           1                             1         0         0        0
#> 274           1                             1         0         0        0
#> 275           1                             1         0         0        0
#> 276           1                             1         0         0        0
#> 277           1                             1         0         0        0
#> 278           1                             1         0         0        0
#> 280           1                             1         0         0        0
#> 281           1                             0         0         0        1
#> 282           1                             0         0         0        1
#> 283           1                             0         0         0        1
#> 284           1                             0         0         0        1
#> 285           1                             0         0         1        1
#> 286           1                             0         0         1        1
#> 287           1                             0         0         1        1
#> 291           1                             0         1         0        0
#> 292           1                             0         1         0        0
#> 293           1                             0         1         1        0
#> 295           1                             0         1         1        0
#> 296           1                             0         1         1        0
#> 298           1                             0         1         0        0
#> 299           1                             0         1         0        0
#> 300           1                             0         1         0        0
#> 301           1                             0         1         0        1
#> 304           1                             0         1         0        1
#> 305           1                             0         0         0        1
#> 306           1                             0         0         0        1
#> 307           1                             0         0         0        1
#> 308           1                             0         0         0        1
#> 310           1                             0         1         1        1
#> 311           1                             0         1         1        1
#> 312           1                             0         1         1        1
#> 316           1                             0         0         1        0
#> 317           1                             0         0         0        1
#> 318           1                             0         0         0        1
#> 319           1                             0         0         0        1
#> 322           1                             0         1         0        0
#> 323           1                             0         1         0        0
#> 324           1                             0         1         0        0
#> 325           1                             1         0         1        0
#> 327           1                             1         0         1        0
#> 328           1                             1         0         1        0
#> 329           1                             0         1         0        1
#> 330           1                             0         1         0        1
#> 331           1                             0         1         0        1
#> 332           1                             0         1         0        1
#> 336           1                             0         0         1        0
#> 339           1                             0         0         1        0
#> 340           1                             0         0         1        0
#> 341           1                             1         0         1        1
#> 342           1                             1         0         1        1
#> 343           1                             1         0         1        1
#> 344           1                             1         0         1        1
#> 345           1                             0         1         0        0
#> 347           1                             0         1         0        0
#> 349           1                             1         0         1        0
#> 351           1                             1         0         1        0
#> 352           1                             1         0         1        0
#> 353           1                             1         0         0        0
#> 354           1                             1         0         0        0
#> 355           1                             1         0         0        0
#> 356           1                             1         0         0        0
#> 357           1                             0         0         0        0
#> 363           1                             0         1         1        1
#> 364           1                             0         1         1        1
#> 365           1                             0         1         1        1
#> 367           1                             0         1         1        1
#> 368           1                             0         1         1        1
#> 370           1                             1         0         0        0
#> 371           1                             1         0         0        0
#> 372           1                             1         0         0        0
#> 373           1                             0         0         1        0
#> 375           1                             0         0         1        0
#> 376           1                             0         0         1        0
#> 378           1                             0         0         1        0
#> 379           1                             0         0         1        0
#> 381           1                             1         0         1        0
#> 382           1                             1         0         1        0
#> 384           1                             1         0         1        0
#> 385           1                             0         1         0        1
#> 386           1                             0         1         0        1
#> 388           1                             0         1         0        1
#> 389           1                             0         0         0        0
#> 390           1                             0         0         0        0
#> 391           1                             0         0         0        0
#> 392           1                             0         0         0        0
#> 394           1                             1         0         0        0
#> 397           1                             1         0         1        0
#> 398           1                             1         0         1        0
#> 399           1                             1         0         1        0
#> 402           1                             0         1         0        1
#> 403           1                             0         1         0        1
#> 405           1                             0         0         1        0
#> 406           1                             0         0         1        0
#> 407           1                             0         0         1        0
#> 408           1                             0         0         1        0
#> 409           1                             0         0         1        0
#> 410           1                             0         0         1        0
#> 411           1                             0         0         1        0
#> 413           1                             1         0         1        0
#> 415           1                             1         0         1        0
#> 416           1                             1         0         1        0
#> 418           1                             1         0         1        1
#> 419           1                             1         0         1        1
#> 421           1                             0         0         1        1
#> 422           1                             0         0         1        1
#> 423           1                             0         0         1        1
#> 424           1                             0         0         1        1
#> 427           1                             0         1         1        0
#> 428           1                             0         1         1        0
#> 429           1                             0         1         0        1
#> 430           1                             0         1         0        1
#> 431           1                             0         1         0        1
#> 432           1                             0         1         0        1
#> 434           1                             1         0         1        0
#> 435           1                             1         0         1        0
#> 436           1                             1         0         1        0
#> 438           1                             0         0         0        1
#> 439           1                             0         0         0        1
#> 444           1                             0         1         1        0
#> 445           1                             1         0         1        0
#> 447           1                             1         0         1        0
#> 449           1                             0         0         0        0
#> 450           1                             0         0         0        0
#> 451           1                             0         0         0        0
#> 453           1                             1         0         0        0
#> 454           1                             1         0         0        0
#> 455           1                             1         0         0        0
#> 456           1                             1         0         0        0
#> 457           1                             0         0         1        0
#> 458           1                             0         0         1        0
#> 459           1                             0         0         1        0
#> 461           1                             1         0         0        1
#> 463           1                             1         0         0        1
#> 464           1                             1         0         0        1
#> 465           1                             1         0         0        0
#> 469           1                             0         0         1        0
#> 470           1                             0         0         1        0
#> 471           1                             0         0         1        0
#> 473           1                             0         1         0        1
#> 474           1                             0         1         0        1
#> 477           1                             0         0         1        0
#> 484           1                             1         0         0        1
#> 487           1                             0         0         1        0
#> 489           1                             0         1         1        0
#> 490           1                             0         1         1        0
#> 491           1                             0         1         1        0
#> 494           1                             0         0         1        1
#> 495           1                             0         0         1        1
#> 496           1                             0         0         1        1
#> 497           1                             0         1         0        0
#> 498           1                             0         1         0        0
#> 499           1                             0         1         0        0
#> 501           1                             0         0         0        1
#> 502           1                             0         0         0        1
#> 504           1                             0         0         0        1
#> 505           1                             1         0         1        1
#> 508           1                             1         0         1        1
#> 509           1                             0         0         1        1
#> 510           1                             0         0         1        1
#> 511           1                             0         0         1        1
#> 512           1                             0         0         1        1
#> 513           1                             0         1         0        1
#> 518           1                             0         1         0        0
#> 519           1                             0         1         0        0
#> 521           1                             0         0         1        0
#> 522           1                             0         0         1        0
#> 523           1                             0         0         1        0
#> 524           1                             0         0         1        0
#> 526           1                             0         0         0        0
#> 527           1                             0         0         0        0
#> 528           1                             0         0         0        0
#> 530           1                             0         1         0        0
#> 531           1                             0         1         0        0
#> 532           1                             0         1         0        0
#> 534           1                             1         0         0        0
#> 535           1                             1         0         0        0
#> 536           1                             1         0         0        0
#> 537           1                             1         0         0        0
#> 538           1                             1         0         0        0
#> 539           1                             1         0         0        0
#> 540           1                             1         0         0        0
#> 541           1                             0         1         1        1
#> 544           1                             0         1         1        1
#> 545           1                             0         0         1        1
#> 546           1                             0         0         1        1
#> 547           1                             0         0         1        1
#> 548           1                             0         0         1        1
#> 549           1                             0         0         0        1
#> 550           1                             0         0         0        1
#> 551           1                             0         0         0        1
#> 555           1                             0         0         1        1
#> 556           1                             0         0         1        1
#> 557           1                             1         0         1        0
#> 558           1                             1         0         1        0
#> 560           1                             1         0         1        0
#> 562           1                             0         1         0        1
#> 564           1                             0         1         0        1
#> 569           1                             1         0         1        1
#> 570           1                             1         0         1        1
#> 572           1                             1         0         1        1
#> 573           1                             0         0         1        0
#> 574           1                             0         0         1        0
#> 575           1                             0         0         1        0
#> 576           1                             0         0         1        0
#> 577           1                             1         0         0        0
#> 578           1                             1         0         0        0
#> 579           1                             1         0         0        0
#> 582           1                             0         0         1        1
#> 583           1                             0         0         1        1
#> 584           1                             0         0         1        1
#> 585           1                             0         0         1        0
#> 586           1                             0         0         1        0
#> 587           1                             0         0         1        0
#> 590           1                             1         0         0        1
#> 591           1                             1         0         0        1
#> 593           1                             0         0         1        0
#> 594           1                             0         0         1        0
#> 595           1                             0         0         1        0
#> 596           1                             0         0         1        0
#> 599           1                             0         1         0        0
#> 600           1                             0         1         0        0
#> 601           1                             0         0         1        0
#> 602           1                             0         0         1        0
#> 604           1                             0         0         1        0
#> 606           1                             0         0         1        1
#> 608           1                             0         0         1        1
#> 609           1                             1         0         0        0
#> 610           1                             1         0         0        0
#> 611           1                             1         0         0        0
#> 612           1                             1         0         0        0
#> 613           1                             1         0         1        0
#> 614           1                             1         0         1        0
#> 616           1                             1         0         1        0
#> 617           1                             0         0         0        1
#> 619           1                             0         0         0        1
#> 620           1                             0         0         0        1
#> 621           1                             0         1         0        0
#> 622           1                             0         1         0        0
#> 623           1                             0         1         0        0
#> 624           1                             0         1         0        0
#> 625           1                             1         0         0        1
#> 628           1                             1         0         0        1
#> 630           1                             1         0         0        1
#> 631           1                             1         0         0        1
#> 632           1                             1         0         0        1
#> 633           1                             0         0         1        1
#> 634           1                             0         0         1        1
#> 638           1                             0         0         0        1
#> 639           1                             0         0         0        1
#> 640           1                             0         0         0        1
#> 642           1                             1         0         1        0
#> 645           1                             0         0         0        0
#> 648           1                             0         0         0        0
#> 650           1                             0         1         0        0
#> 652           1                             0         1         0        0
#> 654           1                             1         0         1        0
#> 655           1                             1         0         1        0
#> 656           1                             1         0         1        0
#> 657           1                             0         1         1        1
#> 661           1                             0         0         0        1
#> 665           1                             1         0         0        0
#> 666           1                             1         0         0        0
#> 668           1                             1         0         0        0
#> 669           1                             1         0         1        0
#> 670           1                             1         0         1        0
#> 671           1                             1         0         1        0
#> 673           1                             0         1         1        0
#> 674           1                             0         1         1        0
#> 678           1                             0         1         1        1
#> 679           1                             0         1         1        1
#> 680           1                             0         1         1        1
#> 682           1                             0         0         1        0
#> 684           1                             0         0         1        0
#> 685           1                             0         0         1        1
#> 686           1                             0         0         1        1
#> 687           1                             0         0         1        1
#> 689           1                             0         1         1        1
#> 690           1                             0         1         1        1
#> 691           1                             0         1         1        1
#> 693           1                             1         0         1        1
#> 694           1                             1         0         1        1
#> 695           1                             1         0         1        1
#> 697           1                             1         0         1        0
#> 698           1                             1         0         1        0
#> 700           1                             1         0         1        0
#> 701           1                             1         0         0        1
#> 702           1                             1         0         0        1
#> 704           1                             1         0         0        1
#> 707           1                             1         0         0        0
#> 709           1                             1         0         0        0
#> 712           1                             1         0         0        0
#> 713           1                             0         1         0        1
#> 715           1                             0         1         0        1
#> 716           1                             0         1         0        1
#> 717           1                             0         0         0        0
#> 718           1                             0         0         0        0
#> 719           1                             0         0         0        0
#> 721           1                             0         0         1        0
#> 723           1                             0         0         1        0
#> 724           1                             0         0         1        0
#> 727           1                             1         0         1        1
#> 728           1                             1         0         1        1
#> 729           1                             1         0         1        1
#> 730           1                             1         0         1        1
#> 734           1                             0         1         1        1
#> 736           1                             0         1         1        1
#> 738           1                             1         0         1        0
#> 739           1                             1         0         1        0
#> 740           1                             1         0         1        0
#> 741           1                             0         1         0        0
#> 742           1                             0         1         0        0
#> 744           1                             0         1         0        0
#> 745           1                             1         0         1        0
#> 749           1                             1         0         1        0
#> 751           1                             1         0         1        0
#> 753           1                             0         0         1        1
#> 754           1                             0         0         1        1
#> 755           1                             0         0         1        1
#> 757           1                             1         0         1        1
#> 758           1                             1         0         1        1
#> 759           1                             1         0         1        1
#> 760           1                             1         0         1        1
#> 761           1                             0         1         0        1
#> 764           1                             0         1         0        1
#> 765           1                             0         1         1        1
#> 766           1                             0         1         1        1
#> 767           1                             0         1         1        1
#> 768           1                             0         1         1        1
#> 770           1                             0         0         0        0
#> 771           1                             0         0         0        0
#> 772           1                             0         0         0        0
#> 773           1                             1         0         0        0
#> 774           1                             1         0         0        0
#> 775           1                             1         0         0        0
#> 776           1                             1         0         0        0
#> 778           1                             0         1         1        1
#> 779           1                             0         1         1        1
#> 781           1                             0         0         0        0
#> 782           1                             0         0         0        0
#> 784           1                             0         0         0        0
#> 788           1                             1         0         0        0
#> 789           1                             1         0         0        1
#> 790           1                             1         0         0        1
#> 792           1                             1         0         0        1
#> 797           1                             1         0         0        0
#> 798           1                             1         0         0        0
#> 800           1                             1         0         0        0
#>     AVISITVIS2 AVISITVIS3 AVISITVIS4 ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
#> 2            1          0          0                   1                   0
#> 4            0          0          1                   0                   0
#> 6            1          0          0                   0                   0
#> 7            0          1          0                   0                   0
#> 8            0          0          1                   0                   0
#> 10           1          0          0                   0                   0
#> 12           0          0          1                   0                   0
#> 13           0          0          0                   0                   0
#> 14           1          0          0                   1                   0
#> 16           0          0          1                   0                   0
#> 17           0          0          0                   0                   0
#> 19           0          1          0                   0                   0
#> 20           0          0          1                   0                   0
#> 23           0          1          0                   0                   0
#> 25           0          0          0                   0                   0
#> 26           1          0          0                   0                   0
#> 28           0          0          1                   0                   0
#> 29           0          0          0                   0                   0
#> 30           1          0          0                   0                   0
#> 31           0          1          0                   0                   0
#> 32           0          0          1                   0                   0
#> 33           0          0          0                   0                   0
#> 34           1          0          0                   1                   0
#> 36           0          0          1                   0                   0
#> 39           0          1          0                   0                   0
#> 41           0          0          0                   0                   0
#> 42           1          0          0                   1                   0
#> 43           0          1          0                   0                   1
#> 44           0          0          1                   0                   0
#> 45           0          0          0                   0                   0
#> 46           1          0          0                   0                   0
#> 47           0          1          0                   0                   0
#> 51           0          1          0                   0                   1
#> 52           0          0          1                   0                   0
#> 55           0          1          0                   0                   0
#> 59           0          1          0                   0                   0
#> 60           0          0          1                   0                   0
#> 62           1          0          0                   0                   0
#> 64           0          0          1                   0                   0
#> 65           0          0          0                   0                   0
#> 68           0          0          1                   0                   0
#> 69           0          0          0                   0                   0
#> 70           1          0          0                   1                   0
#> 72           0          0          1                   0                   0
#> 73           0          0          0                   0                   0
#> 74           1          0          0                   1                   0
#> 75           0          1          0                   0                   1
#> 76           0          0          1                   0                   0
#> 78           1          0          0                   1                   0
#> 79           0          1          0                   0                   1
#> 82           1          0          0                   1                   0
#> 83           0          1          0                   0                   1
#> 84           0          0          1                   0                   0
#> 85           0          0          0                   0                   0
#> 86           1          0          0                   1                   0
#> 87           0          1          0                   0                   1
#> 88           0          0          1                   0                   0
#> 89           0          0          0                   0                   0
#> 90           1          0          0                   0                   0
#> 91           0          1          0                   0                   0
#> 93           0          0          0                   0                   0
#> 94           1          0          0                   1                   0
#> 95           0          1          0                   0                   1
#> 96           0          0          1                   0                   0
#> 97           0          0          0                   0                   0
#> 98           1          0          0                   1                   0
#> 99           0          1          0                   0                   1
#> 100          0          0          1                   0                   0
#> 101          0          0          0                   0                   0
#> 102          1          0          0                   1                   0
#> 103          0          1          0                   0                   1
#> 104          0          0          1                   0                   0
#> 105          0          0          0                   0                   0
#> 107          0          1          0                   0                   1
#> 108          0          0          1                   0                   0
#> 109          0          0          0                   0                   0
#> 110          1          0          0                   1                   0
#> 111          0          1          0                   0                   1
#> 112          0          0          1                   0                   0
#> 113          0          0          0                   0                   0
#> 114          1          0          0                   1                   0
#> 116          0          0          1                   0                   0
#> 117          0          0          0                   0                   0
#> 118          1          0          0                   1                   0
#> 119          0          1          0                   0                   1
#> 120          0          0          1                   0                   0
#> 121          0          0          0                   0                   0
#> 123          0          1          0                   0                   0
#> 125          0          0          0                   0                   0
#> 128          0          0          1                   0                   0
#> 129          0          0          0                   0                   0
#> 130          1          0          0                   1                   0
#> 132          0          0          1                   0                   0
#> 133          0          0          0                   0                   0
#> 134          1          0          0                   0                   0
#> 135          0          1          0                   0                   0
#> 136          0          0          1                   0                   0
#> 137          0          0          0                   0                   0
#> 138          1          0          0                   1                   0
#> 140          0          0          1                   0                   0
#> 142          1          0          0                   1                   0
#> 144          0          0          1                   0                   0
#> 145          0          0          0                   0                   0
#> 146          1          0          0                   0                   0
#> 147          0          1          0                   0                   0
#> 148          0          0          1                   0                   0
#> 149          0          0          0                   0                   0
#> 151          0          1          0                   0                   0
#> 153          0          0          0                   0                   0
#> 155          0          1          0                   0                   0
#> 156          0          0          1                   0                   0
#> 157          0          0          0                   0                   0
#> 158          1          0          0                   0                   0
#> 159          0          1          0                   0                   0
#> 162          1          0          0                   0                   0
#> 163          0          1          0                   0                   0
#> 164          0          0          1                   0                   0
#> 165          0          0          0                   0                   0
#> 168          0          0          1                   0                   0
#> 169          0          0          0                   0                   0
#> 170          1          0          0                   1                   0
#> 171          0          1          0                   0                   1
#> 172          0          0          1                   0                   0
#> 173          0          0          0                   0                   0
#> 177          0          0          0                   0                   0
#> 178          1          0          0                   0                   0
#> 179          0          1          0                   0                   0
#> 180          0          0          1                   0                   0
#> 181          0          0          0                   0                   0
#> 182          1          0          0                   0                   0
#> 183          0          1          0                   0                   0
#> 185          0          0          0                   0                   0
#> 186          1          0          0                   1                   0
#> 187          0          1          0                   0                   1
#> 190          1          0          0                   0                   0
#> 191          0          1          0                   0                   0
#> 193          0          0          0                   0                   0
#> 194          1          0          0                   0                   0
#> 195          0          1          0                   0                   0
#> 197          0          0          0                   0                   0
#> 198          1          0          0                   1                   0
#> 199          0          1          0                   0                   1
#> 201          0          0          0                   0                   0
#> 202          1          0          0                   1                   0
#> 204          0          0          1                   0                   0
#> 206          1          0          0                   1                   0
#> 208          0          0          1                   0                   0
#> 209          0          0          0                   0                   0
#> 210          1          0          0                   0                   0
#> 217          0          0          0                   0                   0
#> 218          1          0          0                   1                   0
#> 219          0          1          0                   0                   1
#> 221          0          0          0                   0                   0
#> 224          0          0          1                   0                   0
#> 226          1          0          0                   1                   0
#> 227          0          1          0                   0                   1
#> 228          0          0          1                   0                   0
#> 230          1          0          0                   1                   0
#> 231          0          1          0                   0                   1
#> 233          0          0          0                   0                   0
#> 235          0          1          0                   0                   1
#> 236          0          0          1                   0                   0
#> 237          0          0          0                   0                   0
#> 238          1          0          0                   1                   0
#> 239          0          1          0                   0                   1
#> 240          0          0          1                   0                   0
#> 241          0          0          0                   0                   0
#> 242          1          0          0                   1                   0
#> 244          0          0          1                   0                   0
#> 246          1          0          0                   0                   0
#> 250          1          0          0                   0                   0
#> 251          0          1          0                   0                   0
#> 252          0          0          1                   0                   0
#> 253          0          0          0                   0                   0
#> 254          1          0          0                   0                   0
#> 256          0          0          1                   0                   0
#> 257          0          0          0                   0                   0
#> 258          1          0          0                   1                   0
#> 259          0          1          0                   0                   1
#> 260          0          0          1                   0                   0
#> 261          0          0          0                   0                   0
#> 262          1          0          0                   1                   0
#> 263          0          1          0                   0                   1
#> 264          0          0          1                   0                   0
#> 265          0          0          0                   0                   0
#> 266          1          0          0                   1                   0
#> 267          0          1          0                   0                   1
#> 268          0          0          1                   0                   0
#> 269          0          0          0                   0                   0
#> 270          1          0          0                   1                   0
#> 273          0          0          0                   0                   0
#> 274          1          0          0                   0                   0
#> 275          0          1          0                   0                   0
#> 276          0          0          1                   0                   0
#> 277          0          0          0                   0                   0
#> 278          1          0          0                   0                   0
#> 280          0          0          1                   0                   0
#> 281          0          0          0                   0                   0
#> 282          1          0          0                   1                   0
#> 283          0          1          0                   0                   1
#> 284          0          0          1                   0                   0
#> 285          0          0          0                   0                   0
#> 286          1          0          0                   1                   0
#> 287          0          1          0                   0                   1
#> 291          0          1          0                   0                   0
#> 292          0          0          1                   0                   0
#> 293          0          0          0                   0                   0
#> 295          0          1          0                   0                   0
#> 296          0          0          1                   0                   0
#> 298          1          0          0                   0                   0
#> 299          0          1          0                   0                   0
#> 300          0          0          1                   0                   0
#> 301          0          0          0                   0                   0
#> 304          0          0          1                   0                   0
#> 305          0          0          0                   0                   0
#> 306          1          0          0                   1                   0
#> 307          0          1          0                   0                   1
#> 308          0          0          1                   0                   0
#> 310          1          0          0                   1                   0
#> 311          0          1          0                   0                   1
#> 312          0          0          1                   0                   0
#> 316          0          0          1                   0                   0
#> 317          0          0          0                   0                   0
#> 318          1          0          0                   1                   0
#> 319          0          1          0                   0                   1
#> 322          1          0          0                   0                   0
#> 323          0          1          0                   0                   0
#> 324          0          0          1                   0                   0
#> 325          0          0          0                   0                   0
#> 327          0          1          0                   0                   0
#> 328          0          0          1                   0                   0
#> 329          0          0          0                   0                   0
#> 330          1          0          0                   1                   0
#> 331          0          1          0                   0                   1
#> 332          0          0          1                   0                   0
#> 336          0          0          1                   0                   0
#> 339          0          1          0                   0                   0
#> 340          0          0          1                   0                   0
#> 341          0          0          0                   0                   0
#> 342          1          0          0                   1                   0
#> 343          0          1          0                   0                   1
#> 344          0          0          1                   0                   0
#> 345          0          0          0                   0                   0
#> 347          0          1          0                   0                   0
#> 349          0          0          0                   0                   0
#> 351          0          1          0                   0                   0
#> 352          0          0          1                   0                   0
#> 353          0          0          0                   0                   0
#> 354          1          0          0                   0                   0
#> 355          0          1          0                   0                   0
#> 356          0          0          1                   0                   0
#> 357          0          0          0                   0                   0
#> 363          0          1          0                   0                   1
#> 364          0          0          1                   0                   0
#> 365          0          0          0                   0                   0
#> 367          0          1          0                   0                   1
#> 368          0          0          1                   0                   0
#> 370          1          0          0                   0                   0
#> 371          0          1          0                   0                   0
#> 372          0          0          1                   0                   0
#> 373          0          0          0                   0                   0
#> 375          0          1          0                   0                   0
#> 376          0          0          1                   0                   0
#> 378          1          0          0                   0                   0
#> 379          0          1          0                   0                   0
#> 381          0          0          0                   0                   0
#> 382          1          0          0                   0                   0
#> 384          0          0          1                   0                   0
#> 385          0          0          0                   0                   0
#> 386          1          0          0                   1                   0
#> 388          0          0          1                   0                   0
#> 389          0          0          0                   0                   0
#> 390          1          0          0                   0                   0
#> 391          0          1          0                   0                   0
#> 392          0          0          1                   0                   0
#> 394          1          0          0                   0                   0
#> 397          0          0          0                   0                   0
#> 398          1          0          0                   0                   0
#> 399          0          1          0                   0                   0
#> 402          1          0          0                   1                   0
#> 403          0          1          0                   0                   1
#> 405          0          0          0                   0                   0
#> 406          1          0          0                   0                   0
#> 407          0          1          0                   0                   0
#> 408          0          0          1                   0                   0
#> 409          0          0          0                   0                   0
#> 410          1          0          0                   0                   0
#> 411          0          1          0                   0                   0
#> 413          0          0          0                   0                   0
#> 415          0          1          0                   0                   0
#> 416          0          0          1                   0                   0
#> 418          1          0          0                   1                   0
#> 419          0          1          0                   0                   1
#> 421          0          0          0                   0                   0
#> 422          1          0          0                   1                   0
#> 423          0          1          0                   0                   1
#> 424          0          0          1                   0                   0
#> 427          0          1          0                   0                   0
#> 428          0          0          1                   0                   0
#> 429          0          0          0                   0                   0
#> 430          1          0          0                   1                   0
#> 431          0          1          0                   0                   1
#> 432          0          0          1                   0                   0
#> 434          1          0          0                   0                   0
#> 435          0          1          0                   0                   0
#> 436          0          0          1                   0                   0
#> 438          1          0          0                   1                   0
#> 439          0          1          0                   0                   1
#> 444          0          0          1                   0                   0
#> 445          0          0          0                   0                   0
#> 447          0          1          0                   0                   0
#> 449          0          0          0                   0                   0
#> 450          1          0          0                   0                   0
#> 451          0          1          0                   0                   0
#> 453          0          0          0                   0                   0
#> 454          1          0          0                   0                   0
#> 455          0          1          0                   0                   0
#> 456          0          0          1                   0                   0
#> 457          0          0          0                   0                   0
#> 458          1          0          0                   0                   0
#> 459          0          1          0                   0                   0
#> 461          0          0          0                   0                   0
#> 463          0          1          0                   0                   1
#> 464          0          0          1                   0                   0
#> 465          0          0          0                   0                   0
#> 469          0          0          0                   0                   0
#> 470          1          0          0                   0                   0
#> 471          0          1          0                   0                   0
#> 473          0          0          0                   0                   0
#> 474          1          0          0                   1                   0
#> 477          0          0          0                   0                   0
#> 484          0          0          1                   0                   0
#> 487          0          1          0                   0                   0
#> 489          0          0          0                   0                   0
#> 490          1          0          0                   0                   0
#> 491          0          1          0                   0                   0
#> 494          1          0          0                   1                   0
#> 495          0          1          0                   0                   1
#> 496          0          0          1                   0                   0
#> 497          0          0          0                   0                   0
#> 498          1          0          0                   0                   0
#> 499          0          1          0                   0                   0
#> 501          0          0          0                   0                   0
#> 502          1          0          0                   1                   0
#> 504          0          0          1                   0                   0
#> 505          0          0          0                   0                   0
#> 508          0          0          1                   0                   0
#> 509          0          0          0                   0                   0
#> 510          1          0          0                   1                   0
#> 511          0          1          0                   0                   1
#> 512          0          0          1                   0                   0
#> 513          0          0          0                   0                   0
#> 518          1          0          0                   0                   0
#> 519          0          1          0                   0                   0
#> 521          0          0          0                   0                   0
#> 522          1          0          0                   0                   0
#> 523          0          1          0                   0                   0
#> 524          0          0          1                   0                   0
#> 526          1          0          0                   0                   0
#> 527          0          1          0                   0                   0
#> 528          0          0          1                   0                   0
#> 530          1          0          0                   0                   0
#> 531          0          1          0                   0                   0
#> 532          0          0          1                   0                   0
#> 534          1          0          0                   0                   0
#> 535          0          1          0                   0                   0
#> 536          0          0          1                   0                   0
#> 537          0          0          0                   0                   0
#> 538          1          0          0                   0                   0
#> 539          0          1          0                   0                   0
#> 540          0          0          1                   0                   0
#> 541          0          0          0                   0                   0
#> 544          0          0          1                   0                   0
#> 545          0          0          0                   0                   0
#> 546          1          0          0                   1                   0
#> 547          0          1          0                   0                   1
#> 548          0          0          1                   0                   0
#> 549          0          0          0                   0                   0
#> 550          1          0          0                   1                   0
#> 551          0          1          0                   0                   1
#> 555          0          1          0                   0                   1
#> 556          0          0          1                   0                   0
#> 557          0          0          0                   0                   0
#> 558          1          0          0                   0                   0
#> 560          0          0          1                   0                   0
#> 562          1          0          0                   1                   0
#> 564          0          0          1                   0                   0
#> 569          0          0          0                   0                   0
#> 570          1          0          0                   1                   0
#> 572          0          0          1                   0                   0
#> 573          0          0          0                   0                   0
#> 574          1          0          0                   0                   0
#> 575          0          1          0                   0                   0
#> 576          0          0          1                   0                   0
#> 577          0          0          0                   0                   0
#> 578          1          0          0                   0                   0
#> 579          0          1          0                   0                   0
#> 582          1          0          0                   1                   0
#> 583          0          1          0                   0                   1
#> 584          0          0          1                   0                   0
#> 585          0          0          0                   0                   0
#> 586          1          0          0                   0                   0
#> 587          0          1          0                   0                   0
#> 590          1          0          0                   1                   0
#> 591          0          1          0                   0                   1
#> 593          0          0          0                   0                   0
#> 594          1          0          0                   0                   0
#> 595          0          1          0                   0                   0
#> 596          0          0          1                   0                   0
#> 599          0          1          0                   0                   0
#> 600          0          0          1                   0                   0
#> 601          0          0          0                   0                   0
#> 602          1          0          0                   0                   0
#> 604          0          0          1                   0                   0
#> 606          1          0          0                   1                   0
#> 608          0          0          1                   0                   0
#> 609          0          0          0                   0                   0
#> 610          1          0          0                   0                   0
#> 611          0          1          0                   0                   0
#> 612          0          0          1                   0                   0
#> 613          0          0          0                   0                   0
#> 614          1          0          0                   0                   0
#> 616          0          0          1                   0                   0
#> 617          0          0          0                   0                   0
#> 619          0          1          0                   0                   1
#> 620          0          0          1                   0                   0
#> 621          0          0          0                   0                   0
#> 622          1          0          0                   0                   0
#> 623          0          1          0                   0                   0
#> 624          0          0          1                   0                   0
#> 625          0          0          0                   0                   0
#> 628          0          0          1                   0                   0
#> 630          1          0          0                   1                   0
#> 631          0          1          0                   0                   1
#> 632          0          0          1                   0                   0
#> 633          0          0          0                   0                   0
#> 634          1          0          0                   1                   0
#> 638          1          0          0                   1                   0
#> 639          0          1          0                   0                   1
#> 640          0          0          1                   0                   0
#> 642          1          0          0                   0                   0
#> 645          0          0          0                   0                   0
#> 648          0          0          1                   0                   0
#> 650          1          0          0                   0                   0
#> 652          0          0          1                   0                   0
#> 654          1          0          0                   0                   0
#> 655          0          1          0                   0                   0
#> 656          0          0          1                   0                   0
#> 657          0          0          0                   0                   0
#> 661          0          0          0                   0                   0
#> 665          0          0          0                   0                   0
#> 666          1          0          0                   0                   0
#> 668          0          0          1                   0                   0
#> 669          0          0          0                   0                   0
#> 670          1          0          0                   0                   0
#> 671          0          1          0                   0                   0
#> 673          0          0          0                   0                   0
#> 674          1          0          0                   0                   0
#> 678          1          0          0                   1                   0
#> 679          0          1          0                   0                   1
#> 680          0          0          1                   0                   0
#> 682          1          0          0                   0                   0
#> 684          0          0          1                   0                   0
#> 685          0          0          0                   0                   0
#> 686          1          0          0                   1                   0
#> 687          0          1          0                   0                   1
#> 689          0          0          0                   0                   0
#> 690          1          0          0                   1                   0
#> 691          0          1          0                   0                   1
#> 693          0          0          0                   0                   0
#> 694          1          0          0                   1                   0
#> 695          0          1          0                   0                   1
#> 697          0          0          0                   0                   0
#> 698          1          0          0                   0                   0
#> 700          0          0          1                   0                   0
#> 701          0          0          0                   0                   0
#> 702          1          0          0                   1                   0
#> 704          0          0          1                   0                   0
#> 707          0          1          0                   0                   0
#> 709          0          0          0                   0                   0
#> 712          0          0          1                   0                   0
#> 713          0          0          0                   0                   0
#> 715          0          1          0                   0                   1
#> 716          0          0          1                   0                   0
#> 717          0          0          0                   0                   0
#> 718          1          0          0                   0                   0
#> 719          0          1          0                   0                   0
#> 721          0          0          0                   0                   0
#> 723          0          1          0                   0                   0
#> 724          0          0          1                   0                   0
#> 727          0          1          0                   0                   1
#> 728          0          0          1                   0                   0
#> 729          0          0          0                   0                   0
#> 730          1          0          0                   1                   0
#> 734          1          0          0                   1                   0
#> 736          0          0          1                   0                   0
#> 738          1          0          0                   0                   0
#> 739          0          1          0                   0                   0
#> 740          0          0          1                   0                   0
#> 741          0          0          0                   0                   0
#> 742          1          0          0                   0                   0
#> 744          0          0          1                   0                   0
#> 745          0          0          0                   0                   0
#> 749          0          0          0                   0                   0
#> 751          0          1          0                   0                   0
#> 753          0          0          0                   0                   0
#> 754          1          0          0                   1                   0
#> 755          0          1          0                   0                   1
#> 757          0          0          0                   0                   0
#> 758          1          0          0                   1                   0
#> 759          0          1          0                   0                   1
#> 760          0          0          1                   0                   0
#> 761          0          0          0                   0                   0
#> 764          0          0          1                   0                   0
#> 765          0          0          0                   0                   0
#> 766          1          0          0                   1                   0
#> 767          0          1          0                   0                   1
#> 768          0          0          1                   0                   0
#> 770          1          0          0                   0                   0
#> 771          0          1          0                   0                   0
#> 772          0          0          1                   0                   0
#> 773          0          0          0                   0                   0
#> 774          1          0          0                   0                   0
#> 775          0          1          0                   0                   0
#> 776          0          0          1                   0                   0
#> 778          1          0          0                   1                   0
#> 779          0          1          0                   0                   1
#> 781          0          0          0                   0                   0
#> 782          1          0          0                   0                   0
#> 784          0          0          1                   0                   0
#> 788          0          0          1                   0                   0
#> 789          0          0          0                   0                   0
#> 790          1          0          0                   1                   0
#> 792          0          0          1                   0                   0
#> 797          0          0          0                   0                   0
#> 798          1          0          0                   0                   0
#> 800          0          0          1                   0                   0
#>     ARMCDTRT:AVISITVIS4
#> 2                     0
#> 4                     1
#> 6                     0
#> 7                     0
#> 8                     0
#> 10                    0
#> 12                    0
#> 13                    0
#> 14                    0
#> 16                    1
#> 17                    0
#> 19                    0
#> 20                    0
#> 23                    0
#> 25                    0
#> 26                    0
#> 28                    0
#> 29                    0
#> 30                    0
#> 31                    0
#> 32                    0
#> 33                    0
#> 34                    0
#> 36                    1
#> 39                    0
#> 41                    0
#> 42                    0
#> 43                    0
#> 44                    1
#> 45                    0
#> 46                    0
#> 47                    0
#> 51                    0
#> 52                    1
#> 55                    0
#> 59                    0
#> 60                    0
#> 62                    0
#> 64                    0
#> 65                    0
#> 68                    0
#> 69                    0
#> 70                    0
#> 72                    1
#> 73                    0
#> 74                    0
#> 75                    0
#> 76                    1
#> 78                    0
#> 79                    0
#> 82                    0
#> 83                    0
#> 84                    1
#> 85                    0
#> 86                    0
#> 87                    0
#> 88                    1
#> 89                    0
#> 90                    0
#> 91                    0
#> 93                    0
#> 94                    0
#> 95                    0
#> 96                    1
#> 97                    0
#> 98                    0
#> 99                    0
#> 100                   1
#> 101                   0
#> 102                   0
#> 103                   0
#> 104                   1
#> 105                   0
#> 107                   0
#> 108                   1
#> 109                   0
#> 110                   0
#> 111                   0
#> 112                   1
#> 113                   0
#> 114                   0
#> 116                   1
#> 117                   0
#> 118                   0
#> 119                   0
#> 120                   1
#> 121                   0
#> 123                   0
#> 125                   0
#> 128                   1
#> 129                   0
#> 130                   0
#> 132                   1
#> 133                   0
#> 134                   0
#> 135                   0
#> 136                   0
#> 137                   0
#> 138                   0
#> 140                   1
#> 142                   0
#> 144                   1
#> 145                   0
#> 146                   0
#> 147                   0
#> 148                   0
#> 149                   0
#> 151                   0
#> 153                   0
#> 155                   0
#> 156                   0
#> 157                   0
#> 158                   0
#> 159                   0
#> 162                   0
#> 163                   0
#> 164                   0
#> 165                   0
#> 168                   0
#> 169                   0
#> 170                   0
#> 171                   0
#> 172                   1
#> 173                   0
#> 177                   0
#> 178                   0
#> 179                   0
#> 180                   0
#> 181                   0
#> 182                   0
#> 183                   0
#> 185                   0
#> 186                   0
#> 187                   0
#> 190                   0
#> 191                   0
#> 193                   0
#> 194                   0
#> 195                   0
#> 197                   0
#> 198                   0
#> 199                   0
#> 201                   0
#> 202                   0
#> 204                   1
#> 206                   0
#> 208                   1
#> 209                   0
#> 210                   0
#> 217                   0
#> 218                   0
#> 219                   0
#> 221                   0
#> 224                   0
#> 226                   0
#> 227                   0
#> 228                   1
#> 230                   0
#> 231                   0
#> 233                   0
#> 235                   0
#> 236                   1
#> 237                   0
#> 238                   0
#> 239                   0
#> 240                   1
#> 241                   0
#> 242                   0
#> 244                   1
#> 246                   0
#> 250                   0
#> 251                   0
#> 252                   0
#> 253                   0
#> 254                   0
#> 256                   0
#> 257                   0
#> 258                   0
#> 259                   0
#> 260                   1
#> 261                   0
#> 262                   0
#> 263                   0
#> 264                   1
#> 265                   0
#> 266                   0
#> 267                   0
#> 268                   1
#> 269                   0
#> 270                   0
#> 273                   0
#> 274                   0
#> 275                   0
#> 276                   0
#> 277                   0
#> 278                   0
#> 280                   0
#> 281                   0
#> 282                   0
#> 283                   0
#> 284                   1
#> 285                   0
#> 286                   0
#> 287                   0
#> 291                   0
#> 292                   0
#> 293                   0
#> 295                   0
#> 296                   0
#> 298                   0
#> 299                   0
#> 300                   0
#> 301                   0
#> 304                   1
#> 305                   0
#> 306                   0
#> 307                   0
#> 308                   1
#> 310                   0
#> 311                   0
#> 312                   1
#> 316                   0
#> 317                   0
#> 318                   0
#> 319                   0
#> 322                   0
#> 323                   0
#> 324                   0
#> 325                   0
#> 327                   0
#> 328                   0
#> 329                   0
#> 330                   0
#> 331                   0
#> 332                   1
#> 336                   0
#> 339                   0
#> 340                   0
#> 341                   0
#> 342                   0
#> 343                   0
#> 344                   1
#> 345                   0
#> 347                   0
#> 349                   0
#> 351                   0
#> 352                   0
#> 353                   0
#> 354                   0
#> 355                   0
#> 356                   0
#> 357                   0
#> 363                   0
#> 364                   1
#> 365                   0
#> 367                   0
#> 368                   1
#> 370                   0
#> 371                   0
#> 372                   0
#> 373                   0
#> 375                   0
#> 376                   0
#> 378                   0
#> 379                   0
#> 381                   0
#> 382                   0
#> 384                   0
#> 385                   0
#> 386                   0
#> 388                   1
#> 389                   0
#> 390                   0
#> 391                   0
#> 392                   0
#> 394                   0
#> 397                   0
#> 398                   0
#> 399                   0
#> 402                   0
#> 403                   0
#> 405                   0
#> 406                   0
#> 407                   0
#> 408                   0
#> 409                   0
#> 410                   0
#> 411                   0
#> 413                   0
#> 415                   0
#> 416                   0
#> 418                   0
#> 419                   0
#> 421                   0
#> 422                   0
#> 423                   0
#> 424                   1
#> 427                   0
#> 428                   0
#> 429                   0
#> 430                   0
#> 431                   0
#> 432                   1
#> 434                   0
#> 435                   0
#> 436                   0
#> 438                   0
#> 439                   0
#> 444                   0
#> 445                   0
#> 447                   0
#> 449                   0
#> 450                   0
#> 451                   0
#> 453                   0
#> 454                   0
#> 455                   0
#> 456                   0
#> 457                   0
#> 458                   0
#> 459                   0
#> 461                   0
#> 463                   0
#> 464                   1
#> 465                   0
#> 469                   0
#> 470                   0
#> 471                   0
#> 473                   0
#> 474                   0
#> 477                   0
#> 484                   1
#> 487                   0
#> 489                   0
#> 490                   0
#> 491                   0
#> 494                   0
#> 495                   0
#> 496                   1
#> 497                   0
#> 498                   0
#> 499                   0
#> 501                   0
#> 502                   0
#> 504                   1
#> 505                   0
#> 508                   1
#> 509                   0
#> 510                   0
#> 511                   0
#> 512                   1
#> 513                   0
#> 518                   0
#> 519                   0
#> 521                   0
#> 522                   0
#> 523                   0
#> 524                   0
#> 526                   0
#> 527                   0
#> 528                   0
#> 530                   0
#> 531                   0
#> 532                   0
#> 534                   0
#> 535                   0
#> 536                   0
#> 537                   0
#> 538                   0
#> 539                   0
#> 540                   0
#> 541                   0
#> 544                   1
#> 545                   0
#> 546                   0
#> 547                   0
#> 548                   1
#> 549                   0
#> 550                   0
#> 551                   0
#> 555                   0
#> 556                   1
#> 557                   0
#> 558                   0
#> 560                   0
#> 562                   0
#> 564                   1
#> 569                   0
#> 570                   0
#> 572                   1
#> 573                   0
#> 574                   0
#> 575                   0
#> 576                   0
#> 577                   0
#> 578                   0
#> 579                   0
#> 582                   0
#> 583                   0
#> 584                   1
#> 585                   0
#> 586                   0
#> 587                   0
#> 590                   0
#> 591                   0
#> 593                   0
#> 594                   0
#> 595                   0
#> 596                   0
#> 599                   0
#> 600                   0
#> 601                   0
#> 602                   0
#> 604                   0
#> 606                   0
#> 608                   1
#> 609                   0
#> 610                   0
#> 611                   0
#> 612                   0
#> 613                   0
#> 614                   0
#> 616                   0
#> 617                   0
#> 619                   0
#> 620                   1
#> 621                   0
#> 622                   0
#> 623                   0
#> 624                   0
#> 625                   0
#> 628                   1
#> 630                   0
#> 631                   0
#> 632                   1
#> 633                   0
#> 634                   0
#> 638                   0
#> 639                   0
#> 640                   1
#> 642                   0
#> 645                   0
#> 648                   0
#> 650                   0
#> 652                   0
#> 654                   0
#> 655                   0
#> 656                   0
#> 657                   0
#> 661                   0
#> 665                   0
#> 666                   0
#> 668                   0
#> 669                   0
#> 670                   0
#> 671                   0
#> 673                   0
#> 674                   0
#> 678                   0
#> 679                   0
#> 680                   1
#> 682                   0
#> 684                   0
#> 685                   0
#> 686                   0
#> 687                   0
#> 689                   0
#> 690                   0
#> 691                   0
#> 693                   0
#> 694                   0
#> 695                   0
#> 697                   0
#> 698                   0
#> 700                   0
#> 701                   0
#> 702                   0
#> 704                   1
#> 707                   0
#> 709                   0
#> 712                   0
#> 713                   0
#> 715                   0
#> 716                   1
#> 717                   0
#> 718                   0
#> 719                   0
#> 721                   0
#> 723                   0
#> 724                   0
#> 727                   0
#> 728                   1
#> 729                   0
#> 730                   0
#> 734                   0
#> 736                   1
#> 738                   0
#> 739                   0
#> 740                   0
#> 741                   0
#> 742                   0
#> 744                   0
#> 745                   0
#> 749                   0
#> 751                   0
#> 753                   0
#> 754                   0
#> 755                   0
#> 757                   0
#> 758                   0
#> 759                   0
#> 760                   1
#> 761                   0
#> 764                   1
#> 765                   0
#> 766                   0
#> 767                   0
#> 768                   1
#> 770                   0
#> 771                   0
#> 772                   0
#> 773                   0
#> 774                   0
#> 775                   0
#> 776                   0
#> 778                   0
#> 779                   0
#> 781                   0
#> 782                   0
#> 784                   0
#> 788                   0
#> 789                   0
#> 790                   0
#> 792                   1
#> 797                   0
#> 798                   0
#> 800                   0
#> attr(,"assign")
#>  [1] 0 1 1 2 3 4 4 4 5 5 5
#> attr(,"contrasts")
#> attr(,"contrasts")$RACE
#>                           Black or African American White
#> Asian                                             0     0
#> Black or African American                         1     0
#> White                                             0     1
#> 
#> attr(,"contrasts")$SEX
#>        Female
#> Male        0
#> Female      1
#> 
#> attr(,"contrasts")$ARMCD
#>     TRT
#> PBO   0
#> TRT   1
#> 
#> attr(,"contrasts")$AVISIT
#>      VIS2 VIS3 VIS4
#> VIS1    0    0    0
#> VIS2    1    0    0
#> VIS3    0    1    0
#> VIS4    0    0    1
#> 
# terms:
terms(object)
#> FEV1 ~ RACE + SEX + ARMCD + AVISIT + ARMCD:AVISIT
#> attr(,"variables")
#> list(FEV1, RACE, SEX, ARMCD, AVISIT)
#> attr(,"factors")
#>        RACE SEX ARMCD AVISIT ARMCD:AVISIT
#> FEV1      0   0     0      0            0
#> RACE      1   0     0      0            0
#> SEX       0   1     0      0            0
#> ARMCD     0   0     1      0            1
#> AVISIT    0   0     0      1            1
#> attr(,"term.labels")
#> [1] "RACE"         "SEX"          "ARMCD"        "AVISIT"       "ARMCD:AVISIT"
#> attr(,"order")
#> [1] 1 1 1 1 2
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: 0x5629a7790798>
terms(object, include = "subject_var")
#> ~RACE + SEX + ARMCD + AVISIT + USUBJID + ARMCD:AVISIT
#> attr(,"variables")
#> list(RACE, SEX, ARMCD, AVISIT, USUBJID)
#> attr(,"factors")
#>         RACE SEX ARMCD AVISIT USUBJID ARMCD:AVISIT
#> RACE       1   0     0      0       0            0
#> SEX        0   1     0      0       0            0
#> ARMCD      0   0     1      0       0            1
#> AVISIT     0   0     0      1       0            1
#> USUBJID    0   0     0      0       1            0
#> attr(,"term.labels")
#> [1] "RACE"         "SEX"          "ARMCD"        "AVISIT"       "USUBJID"     
#> [6] "ARMCD:AVISIT"
#> attr(,"order")
#> [1] 1 1 1 1 1 2
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 0
#> attr(,".Environment")
#> <environment: 0x5629a7790798>
# Log likelihood given the estimated parameters:
logLik(object)
#> 'log Lik.' -1693.225 (df=10)
# Formula which was used:
formula(object)
#> FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> <environment: 0x5629a7790798>
# Variance-covariance matrix estimate for coefficients:
vcov(object)
#>                               (Intercept) RACEBlack or African American
#> (Intercept)                     0.7859971                  -0.226212328
#> RACEBlack or African American  -0.2262123                   0.389969478
#> RACEWhite                      -0.1771113                   0.181466304
#> SEXFemale                      -0.1684152                   0.031537926
#> ARMCDTRT                       -0.5674809                   0.028374129
#> AVISITVIS2                     -0.4227565                   0.002972514
#> AVISITVIS3                     -0.5231223                   0.010825469
#> AVISITVIS4                     -0.4406442                   0.002205681
#> ARMCDTRT:AVISITVIS2             0.4225282                   0.005382569
#> ARMCDTRT:AVISITVIS3             0.5218971                   0.011420575
#> ARMCDTRT:AVISITVIS4             0.4489247                  -0.012589283
#>                                  RACEWhite    SEXFemale     ARMCDTRT
#> (Intercept)                   -0.177111308 -0.168415217 -0.567480906
#> RACEBlack or African American  0.181466304  0.031537926  0.028374129
#> RACEWhite                      0.443035801  0.023364777 -0.042968995
#> SEXFemale                      0.023364777  0.282971189  0.001814594
#> ARMCDTRT                      -0.042968995  0.001814594  1.153791725
#> AVISITVIS2                    -0.003149280  0.006471853  0.419528600
#> AVISITVIS3                    -0.002952986  0.006771404  0.517277529
#> AVISITVIS4                    -0.008230720  0.004088901  0.440653554
#> ARMCDTRT:AVISITVIS2            0.013485683 -0.016801299 -0.845758354
#> ARMCDTRT:AVISITVIS3            0.006720617 -0.024696304 -1.044355829
#> ARMCDTRT:AVISITVIS4            0.002967665 -0.009038640 -0.877606881
#>                                 AVISITVIS2   AVISITVIS3   AVISITVIS4
#> (Intercept)                   -0.422756455 -0.523122299 -0.440644229
#> RACEBlack or African American  0.002972514  0.010825469  0.002205681
#> RACEWhite                     -0.003149280 -0.002952986 -0.008230720
#> SEXFemale                      0.006471853  0.006771404  0.004088901
#> ARMCDTRT                       0.419528600  0.517277529  0.440653554
#> AVISITVIS2                     0.642749706  0.399048940  0.368340113
#> AVISITVIS3                     0.399048940  0.676823960  0.401800094
#> AVISITVIS4                     0.368340113  0.401800094  1.723478787
#> ARMCDTRT:AVISITVIS2           -0.643020114 -0.399203255 -0.368624024
#> ARMCDTRT:AVISITVIS3           -0.399238901 -0.676484876 -0.401792995
#> ARMCDTRT:AVISITVIS4           -0.368506585 -0.402167824 -1.723586879
#>                               ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
#> (Intercept)                           0.422528163         0.521897062
#> RACEBlack or African American         0.005382569         0.011420575
#> RACEWhite                             0.013485683         0.006720617
#> SEXFemale                            -0.016801299        -0.024696304
#> ARMCDTRT                             -0.845758354        -1.044355829
#> AVISITVIS2                           -0.643020114        -0.399238901
#> AVISITVIS3                           -0.399203255        -0.676484876
#> AVISITVIS4                           -0.368624024        -0.401792995
#> ARMCDTRT:AVISITVIS2                   1.275359305         0.805849821
#> ARMCDTRT:AVISITVIS3                   0.805849821         1.410501907
#> ARMCDTRT:AVISITVIS4                   0.728711516         0.796418986
#>                               ARMCDTRT:AVISITVIS4
#> (Intercept)                           0.448924745
#> RACEBlack or African American        -0.012589283
#> RACEWhite                             0.002967665
#> SEXFemale                            -0.009038640
#> ARMCDTRT                             -0.877606881
#> AVISITVIS2                           -0.368506585
#> AVISITVIS3                           -0.402167824
#> AVISITVIS4                           -1.723586879
#> ARMCDTRT:AVISITVIS2                   0.728711516
#> ARMCDTRT:AVISITVIS3                   0.796418986
#> ARMCDTRT:AVISITVIS4                   3.425654435
# Variance-covariance matrix estimate for residuals:
VarCorr(object)
#>           VIS1      VIS2       VIS3       VIS4
#> VIS1 40.553664 14.396045  4.9747288 13.3866534
#> VIS2 14.396045 26.571483  2.7854661  7.4744790
#> VIS3  4.974729  2.785466 14.8978517  0.9082111
#> VIS4 13.386653  7.474479  0.9082111 95.5568420
# REML criterion (twice the negative log likelihood):
deviance(object)
#> [1] 3386.45
# AIC:
AIC(object)
#> [1] 3406.45
AIC(object, corrected = TRUE)
#> [1] 3406.877
# BIC:
BIC(object)
#> [1] 3439.282
# residuals:
residuals(object, type = "response")
#>   [1]  -1.234879652 -31.602602517  -4.161845076  -4.240694906   2.976720902
#>   [6]  -1.486633993 -10.523496721  -0.985476011  -5.929054895   3.894654785
#>  [11]   0.005884300   4.143525922  -5.650335904  -3.625861923   0.826964231
#>  [16]  -3.033654140   2.122875942  -0.077764040  -1.236764407   2.898887820
#>  [21]   5.666894712   6.973709346   1.649933932   2.223736062   1.999981925
#>  [26]   9.449780374  -0.697302530  -0.797571139  -4.121957765   9.568283755
#>  [31]   7.148615054  -1.008039398   3.474215960   0.203016560  -0.741716375
#>  [36]  -6.462963308  -6.753458982  -0.047005589   1.515200267 -14.092698990
#>  [41] -10.947245143  -1.951225988  -5.706079812  -9.307052363  -2.403398748
#>  [46]   7.086671267  -2.850398916  16.073985815   2.634589860   3.803878882
#>  [51]  11.652502799  -0.134982175   4.531571586   5.789979573   6.916172244
#>  [56]  -8.335823810  -2.446122835  -8.765445294  -1.487993507   0.630113879
#>  [61] -10.923600038  -9.812114797   5.259684070  -4.094159929  -8.824895907
#>  [66] -13.157401791   0.877589312  -0.339592573   8.595725270   3.936442210
#>  [71]   1.552353395  13.791819037 -14.336580048   0.027233096   0.336487818
#>  [76]   1.612454504   3.714131743   6.358341554   6.469275659   1.146036288
#>  [81]  -6.481239981 -15.420209264  10.564048465   1.873040490  -0.604472161
#>  [86]  11.276535944  -3.846981011   4.205682602  -2.888867642 -10.092066250
#>  [91]  -8.954675960  -9.131288225 -10.160328309   8.348966008  -3.324891902
#>  [96]  -6.821197849  -0.252287406  -0.230464002  -1.407973607  -2.003675274
#> [101]   4.967728097 -10.820604863  -2.685460226  -1.400767701  -5.913374081
#> [106]   5.965692909   7.081065238   5.753275316   6.224314102   1.714833437
#> [111]  -4.753946846  -2.150649979  -1.304036547  -1.697593065   4.231714405
#> [116]   7.523067828  -2.009615920   6.756933384  -5.242275398  -6.597847500
#> [121]   1.364617396   1.759820544  15.973845801  -0.461493245   2.787801686
#> [126]   0.484952966  -3.870461666  12.311298679 -11.232317044  -4.481649361
#> [131]  -0.220037106 -12.363619506   1.709197627  -6.875616999   5.976752937
#> [136]  -0.656597193   7.283701781  -2.922519989   6.370716054  -5.058284328
#> [141]  -9.236842335  -4.345245889  -9.622488058  -6.654774813 -12.022180587
#> [146]   1.887357480  -6.370718385   4.328882117   3.485715939   5.595034537
#> [151]   9.409222502   2.234484592  -2.968076876   0.569772634   0.236447831
#> [156]   1.328254786   0.485660327   8.823363223   0.276419270  -2.984758150
#> [161]  -0.711357559   2.929205179   3.904409284  -1.545435261   4.840564762
#> [166]   4.044772166  -1.721893559   7.189004781   1.582814876  -6.023735519
#> [171]   9.469873359   3.411370689  12.312877955  -0.415604291  -7.826304065
#> [176]  -2.588748144   4.878824125  -2.386492192   0.275251132  15.403304358
#> [181]  -1.117311743  -5.778945571  -4.284700514  -0.658853762  -1.604989482
#> [186]  -8.628885781  -4.503110575  -4.125143859   1.692779577  -0.289986567
#> [191]  -1.324600198   7.581755477  -1.962979124 -12.646575009  -5.000450448
#> [196]   0.168281529  -2.531874945  -1.621285472   5.569746459   1.486223742
#> [201]  15.758167838  11.723532098   1.222430146   2.140888226  -2.930455637
#> [206]  -7.358901274   1.549014295   4.296486376   4.408791522   2.197562650
#> [211]  -8.375741822   4.953239770  -1.144775488  -1.781395739  -3.146498706
#> [216]   6.783928757   1.098312860 -22.160474609  -2.816167884  -3.716081684
#> [221]   8.774205403  -2.178968843   0.782958690  -0.005585283  -2.923802312
#> [226]  -1.430045436  -3.266421082  -7.413799193   8.803384810   3.193392733
#> [231]   6.552308299  -3.576957870  -2.900210364   0.851863016  -4.147762312
#> [236]   7.742038570  -1.501447881  10.267382270   5.455583260  -6.641733868
#> [241]  -7.367427023  10.801038359  -7.567612905   2.531801030  -3.893743195
#> [246]   0.623788663   9.698229358   3.060268037   5.913536244 -11.374576033
#> [251]   6.770578888  -4.806976192   1.005172447  -7.759025307   2.809952061
#> [256]   5.765703310  -2.046337220   3.454956181   1.797059893  -6.820263102
#> [261]   2.852094314   2.232372715  -3.397204643   6.883657677  -1.853470422
#> [266]   0.858119403  -2.080961909  -5.325272886   8.348411771  -1.049269637
#> [271]  -7.961356400 -10.048198957  -7.611069668  -0.927034289  -8.037769563
#> [276]  -0.395791510   1.964185716   1.846717422  -2.319131312  -1.960386265
#> [281]   4.813456009   4.453877656   7.759026726   1.075918408   8.735930253
#> [286]   0.931062886  -6.492053277   3.905731110   6.103802886  -1.553315409
#> [291]  -0.364087540   6.347166038   3.008394981  -5.651857454   0.406319494
#> [296]   1.155221279  -9.081862494  -4.569513438  17.559986122   2.202330352
#> [301]  -1.269170427  -0.367685386  -3.931511724   3.117376232  -2.997817660
#> [306] -15.997444012  -2.144194495   2.087262596 -10.213796246  -0.460389924
#> [311]  -2.282396185   1.510231160   6.144987953  -1.051913357  -3.165847388
#> [316]   2.362323843   0.673403098  -0.194314949   9.826663341   6.250933707
#> [321]  -0.235076130   2.461097100  -1.767393574  -9.083814120  -9.512139613
#> [326]   0.332052131   2.907510403   6.797230637   4.517753563   6.860770097
#> [331]  -0.535968343   8.144395143   8.315853379  10.472748395  -1.241441160
#> [336]   1.208712197   4.720909904  -2.811983812  -3.180543134   5.616929260
#> [341]  -3.699626586  -1.645226149   0.073590117   5.932694905  13.347774651
#> [346]  -0.599486961   0.586742528   1.009572681  -0.943207940   2.177417986
#> [351]   3.098085788  -3.479836872   0.282540942   4.915937135  -3.702232983
#> [356]  -5.607957192  -3.714729352 -17.040753161  -3.528768581   0.541082039
#> [361]   8.076779586  -6.191258080   0.412993243   5.018532666   1.732496518
#> [366]   4.890613044  -3.824823997  -0.487435234   2.480592663   2.305343529
#> [371] -26.246442718  -5.774618924   0.493027338 -12.146505999  -7.174675073
#> [376]  -2.154139764  -7.657419221  21.074111704   6.040615383   8.467296562
#> [381] -10.336888517  -4.958491116  -3.742055183   0.987848599   2.215631522
#> [386]  -0.851264659  -0.630616202  -9.028257544  -7.573424167 -12.742297395
#> [391]  -4.117784562 -11.901375893   0.720831832  -1.403633842  -0.753280271
#> [396]   7.279393940   1.453345956  -1.800975371   3.792088067  -0.340693264
#> [401]  10.842614257   3.683774850   5.251983786   2.872680103   1.658098849
#> [406]   1.330582219   7.128499486   1.549862924   7.670151870   3.884868504
#> [411]  11.965570612   3.385952871   4.137436188   1.308094515  -2.558461227
#> [416] -14.304693302  -3.106269989  -5.609838571  -0.293261124  17.421651070
#> [421]   0.093535126   0.026595297   9.350679413   1.773045693  -2.732879905
#> [426]   8.784278133  -6.271337496  -6.343229393   5.375845601   7.263453790
#> [431]  -0.250360213   4.653760313   2.676065485  -1.467432090   7.495460612
#> [436]  -6.404623267   7.800378199   7.125463247   7.026637291  -4.402064897
#> [441]   1.594201571  -0.782053228   8.346582123   3.296803523  11.120854597
#> [446]  -1.989665587   1.101524949  -1.109564945   7.158458483 -12.397318171
#> [451]   1.969677544  -0.246975211  -6.819019339  -3.539097324  -0.255945131
#> [456]   0.108762339  -9.625363156  -7.467529543  -4.516696858  -4.277072142
#> [461] -12.507932515  -6.714436823   9.526182803  -2.970783609  -2.364822030
#> [466]  -3.770735866   1.673403803  -0.446717846  -2.617775570  14.220675868
#> [471]   4.269581680   2.564985116  -2.969110991  -2.899565420  -9.571178045
#> [476]  -2.310163663  -6.618388134   7.050040894  -2.768894832  -0.680898398
#> [481]   0.858618471   2.393017430  -0.511076204  -2.130084682  -1.058901346
#> [486]  -5.160549124  -2.821587861   5.706858994   0.904798206  -6.758830844
#> [491]   3.679593184 -10.505575302   7.180743205  -1.038313940  -4.236938656
#> [496]  13.174628062   3.802619676   1.785232749  -7.989780520   2.021917955
#> [501]   6.945228489 -15.967590464  -0.552509306  12.058523414  -0.789256094
#> [506]   2.139638364  -1.406232324   4.761861929   4.051265895   3.897435776
#> [511]  -0.474197410  10.883502127  -9.412746363 -11.176728898  -7.796425447
#> [516]   0.468028997  -1.420902629  27.885028986  -5.422118623  -4.333853988
#> [521]  15.204501679 -11.940481268  -1.922768210  -5.221617225 -17.156866508
#> [526]   3.809621210  -2.857420361 -11.493590753  -5.610248116   3.386308297
#> [531]  -7.228339769   6.263138946  11.445882539  17.502205607   3.395436537
#> [536]   4.496978998   6.888939057
residuals(object, type = "pearson")
#>   [1] -0.2395613408 -3.2328976424 -0.8073800434 -1.0986900855  0.3045139709
#>   [6] -0.2884006003 -1.0765375327 -0.1547501250 -1.1502111470  0.3984171957
#>  [11]  0.0009240166  1.0735152964 -0.5780206744 -0.9393975831  0.1298588873
#>  [16] -0.5885158546  0.2171669446 -0.0122113525 -0.2399269752  0.7510512728
#>  [21]  0.5797146151  1.0950874316  0.3200800859  0.2274847797  0.5181604338
#>  [26]  1.4839069433 -0.1352736914 -0.2066367711 -0.4216699411  1.5025156286
#>  [31]  1.3868005721 -0.2611654261  0.9001087591  0.0207682819 -0.1921657760
#>  [36] -1.6744410682 -0.6908684691 -0.0091188820  0.1550026574 -2.2129883504
#>  [41] -1.1198863446 -0.3064026546 -1.1069549401 -0.9520971453 -0.3774077226
#>  [46]  1.3747837439 -0.7384886436  1.6443440320  0.5110991006  0.9855186723
#>  [51]  2.2605354222 -0.0349715273  0.4635728051  0.9092053518  1.3417076669
#>  [56] -2.1596665585 -0.2502346046 -1.3764452313 -0.2886643401  0.1632515159
#>  [61] -1.7153420821 -1.9035080658  1.3626924050 -0.4188262652 -1.3857808111
#>  [66] -2.5524793535  0.2273680841 -0.0347397980  1.3497939535  0.7636528569
#>  [71]  0.4021876891  1.4108818799 -2.2512851976  0.0070556201  0.0344221864
#>  [76]  0.2532050841  0.7205255825  1.6473353965  0.6617969521  0.1799630402
#>  [81] -1.2573326786 -1.5774636961  1.6588813968  0.3633617985 -0.1566081939
#>  [86]  1.1535722873 -0.6040946569  1.0896190080 -0.4536413104 -1.0324028588
#>  [91] -1.4061602817 -1.7714306394 -1.0393859625  1.3110451395 -0.6450147276
#>  [96] -1.7672533895 -0.0258086137 -0.0361899557 -0.2731408237 -0.2049728995
#> [101]  0.9637178832 -1.1069312387 -0.4217000731 -0.2717429089 -1.5320520854
#> [106]  0.6102812112  1.1119456174  1.4905732876  0.9774092675  0.4442834340
#> [111] -0.4863214523 -0.3377183713 -0.2529774810 -0.4398167544  0.8209343927
#> [116]  1.9490956593 -0.2055806185  1.0610469204 -0.5362767122 -1.0360655304
#> [121]  0.2647299052  0.4559388087  1.6340998625 -0.0724686715  0.4377708385
#> [126]  0.0940787894 -1.0027691104  1.2594269238 -1.7638201727 -0.8694206991
#> [131] -0.0570077764 -1.9414695478  0.3315769879 -1.7813524423  1.1594643621
#> [136] -0.1701128805  1.1437658039 -0.5669563073  1.6505414137 -0.7943066334
#> [141] -1.7919076827 -1.1257774216 -1.5110273758 -1.2909976896 -1.2298505876
#> [146]  0.3661392331 -0.6517146946  0.6797679920  0.6762138991  0.8785929692
#> [151]  1.8253486935  0.5789159848 -0.4660796030  0.0582868643  0.0458698622
#> [156]  0.3441276480  0.0496823046  1.7116945134  0.0716154116 -0.4686990773
#> [161] -0.1843003363  0.2996531856  0.6131126668 -0.2998077932  1.2541059023
#> [166]  0.4137739729 -0.2703903907  1.3946360054  0.1619195279 -1.1685787807
#> [171]  1.8371146990  0.8838266455  1.2595884814 -0.0652626906 -1.5182693254
#> [176] -0.2648249544  0.7661258472 -0.4629692202  0.0713127674  1.5757343502
#> [181] -0.1754524008 -1.1210905839 -1.1100911709 -0.0673997268 -0.2520328454
#> [186] -1.6739667261 -1.1666774083 -0.4219958735  0.2658185978 -0.0562561468
#> [191] -0.2080030808  1.4708279512 -0.5085736534 -1.2937251768 -0.7852249307
#> [196]  0.0326458929 -0.2590069136 -0.2545918184  1.0805068560  0.3850546492
#> [201]  1.6120363386  1.8409560848  0.2371461903  0.5546667988 -0.7592299434
#> [206] -0.7528042846  0.2432430148  1.1131446822  0.4510125934  0.4263177017
#> [211] -2.1700086192  0.5067088119 -0.1797650557 -0.1822340449 -0.4940973327
#> [216]  1.3160529992  0.2845537057 -2.2669824765 -0.5463244563 -0.9627719496
#> [221]  0.8975877207 -0.2229051620  0.1229486602 -0.0010835209 -0.7575061831
#> [226] -0.2774226634 -0.8462727304 -0.7584202573  1.3824029054  0.8273523589
#> [231]  0.6702910636 -0.5616926964 -0.5626283359  0.2207028495 -0.4243097065
#> [236]  0.7919986408 -0.3889989582  1.0503374182  0.8566948182 -1.2884678024
#> [241] -1.9087718415  1.1049296155 -1.1883486059  0.6559454881 -0.6114377619
#> [246]  0.1616127626  0.9921139504  0.4805564584  1.1472005954 -2.9469542586
#> [251]  0.6926198092 -0.7548435060  0.2604226491 -0.7937363579  0.4412491305
#> [256]  1.4937931640 -0.2093371509  0.6702466383  0.4655868747 -0.6977024279
#> [261]  0.4478667638  0.5783688342 -0.3475288111  1.3353999808 -0.4802018588
#> [266]  0.1347512101 -0.4036976594 -0.5447672276  1.3109581072 -0.2035537964
#> [271] -0.8144345176 -1.5778771157 -1.4765147781 -0.2401784153 -0.8222514667
#> [276] -0.0767818506  0.3084377315  0.3582552366 -0.6008464735 -0.3803065033
#> [281]  1.2470824973  0.6993961465  1.5052178110  0.2787516936  0.8936722317
#> [286]  0.1462055864 -1.2594304114  1.0119068082  0.9584852901 -0.4024369301
#> [291] -0.0372455955  1.2313229104  0.7794226682 -0.8875159196  0.0788242342
#> [296]  0.2992976843 -0.9290605680 -1.1838812319  1.7963595784  0.3458337835
#> [301] -0.2462136037 -0.0952608705 -0.4021876039  0.6047575802 -0.7766822687
#> [306] -1.6365139232 -0.4159645093  0.5407733334 -1.0448556503 -0.0722954162
#> [311] -0.5913290428  0.2371528666  1.1921012315 -0.2725324036 -0.4971356724
#> [316]  0.4582806645  0.1744669975 -0.0198781205  1.5430891919  1.2126542520
#> [321] -0.0609041252  0.3864681432 -0.4579008486 -0.9292602164 -1.4936992670
#> [326]  0.0521424248  0.5640445122  1.7610439030  0.7094266338  1.3309598886
#> [331] -0.0841635588  0.8331590995  2.1544925682  1.6445444703 -0.2408342453
#> [336]  0.3131562482  0.9158362154 -0.7285359600 -0.3253646719  0.8820311160
#> [341] -0.7177116444 -0.4262493998  0.0115559178  1.1509172932  1.3654568220
#> [346] -0.0941379407  0.0600228584  0.1585340454 -0.1829782834  0.5641310225
#> [351]  0.3169294122 -0.5464417046  0.0548117275  1.2736335696 -0.5813647525
#> [356] -1.0879195738 -0.9624215841 -1.7432428449 -0.6845659265  0.1401849188
#> [361]  0.8262421320 -1.2010774371  0.1069993459  0.5133881748  0.3360968725
#> [366]  1.2670725393 -0.3912736134 -0.0765423638  0.4812243068  0.5972742993
#> [371] -2.6849707310 -0.9067932563  0.0504359386 -1.9073760316 -1.3918561038
#> [376] -0.5581000410 -0.7833422116  3.3092854483  1.1718534019  2.1937288553
#> [381] -2.6781075222 -0.5072460165 -0.5876180661  0.1916383791  0.2266556978
#> [386] -0.1651416824 -0.0645110677 -1.4177148596 -1.4692117129 -1.3035174298
#> [391] -0.6466191658 -2.3088157318  0.1867549553 -0.1435895837 -0.1182882332
#> [396]  1.4121711135  0.3765365884 -0.3493814754  0.9824638777 -0.0348523970
#> [401]  1.7026248170  0.7146364760  1.3606973957  0.5572875838  0.4295844918
#> [406]  0.2089424426  1.3828982386  0.4015424514  0.7846447419  1.0065016707
#> [411]  1.2240594742  0.5316990211  0.8026448241  0.1338160574 -0.4963304733
#> [416] -1.4633481285 -0.4877801835 -1.0882845531 -0.0759788422  1.7822081153
#> [421]  0.0146878993  0.0051593732  0.9565601255  0.2784228533 -0.7080415173
#> [426]  0.8986181455 -0.9847933905 -1.2305592182  1.3927878315  0.7430401538
#> [431] -0.0393142744  0.4760725239  0.5191451935 -0.3801860600  0.7667740949
#> [436] -1.0057233670  1.5132398188  1.3823092213  1.8204791659 -0.4503244699
#> [441]  0.3092682477 -0.1228064748  0.8538425167  0.6395657029  1.1376463248
#> [446] -0.3859865656  0.2853859017 -0.1135067877  1.1240987437 -1.9467612774
#> [451]  0.3093001178 -0.0479121287 -0.6975751929 -0.5557474232 -0.0496522545
#> [456]  0.0281784250 -1.5114788550 -1.4486686114 -0.8762197629 -1.1081147928
#> [461] -1.2795422629 -1.3025718628  0.9745138524 -0.4665046434 -0.4587653020
#> [466] -0.9769318949  0.2627760037 -0.0866613407 -0.6782199917  2.2330846670
#> [471]  0.8282804812  0.6645429059 -0.4662419908 -0.5625032196 -0.9791167965
#> [476] -0.3627669389 -1.2839388303  0.7212083426 -0.7173723569 -0.1069220469
#> [481]  0.0878353493  0.3757775355 -0.1324109304 -0.2179043876 -0.1662801672
#> [486] -1.0011243330 -0.7310242017  0.8961528576  0.2344174340 -0.6914180023
#> [491]  0.9533184156 -1.0747041990  1.1275981322 -0.2014284382 -0.8219478749
#> [496]  1.3477441922  0.7376918609  0.4625226679 -0.8173422614  0.3175034734
#> [501]  1.3473444535 -1.6334599512 -0.0867610000  1.8935600524 -0.2044824880
#> [506]  0.3359892081 -0.2728030222  1.2337153709  0.6361736837  0.7560857764
#> [511] -0.1228562780  1.1133655321 -1.4780914615 -1.1433621798 -1.2242792315
#> [516]  0.0907956122 -0.3681310883  2.8525955866 -1.0518676907 -1.1228259787
#> [521]  1.5553971419 -1.8750237951 -0.3730087623 -1.3528299493 -1.7551210618
#> [526]  0.7390501284 -0.7403077773 -1.8048482025 -1.0883640031  0.3464141317
#> [531] -0.7394480434  0.9835059653  2.2204519805  1.7904487206  0.5331882492
#> [536]  0.8723945829  0.7047278725
residuals(object, type = "normalized")
#>   [1] -0.2395613408 -3.2331297472 -0.8073800434 -0.9954606428  0.4323839479
#>   [6] -0.2884006003 -1.0453219149 -0.1547501250 -1.2043371512  0.5171524003
#>  [11]  0.0009240166  1.0960105881 -0.5698974273 -0.9393975831  0.1298588873
#>  [16] -0.7182164725  0.2384382028 -0.0122113525 -0.2610103997  0.7859763110
#>  [21]  0.6325743182  1.0950874316 -0.1782248309  0.0027778435  0.5181604338
#>  [26]  1.4839069433 -0.8746381857 -0.4675512449 -0.7174517088  1.5025156286
#>  [31]  0.8099093909 -0.6254223667  0.9001087591 -0.0008984953 -0.1921657760
#>  [36] -1.6744410682 -0.6507515544 -0.0091188820  0.1581043716 -2.2129883504
#>  [41] -0.6594254412 -0.3064026546 -1.0822020259 -0.8424181978 -0.3774077226
#>  [46]  1.7139041454 -0.7772005644  1.6463084891  0.5110991006  0.9230554915
#>  [51]  2.2605354222 -0.3549422277  0.1308906429  0.9092053518  1.0492579025
#>  [56] -2.4584698201 -0.5812031216 -1.3764452313  0.3504785963  0.4314962476
#>  [61] -1.7153420821 -1.2810001583  1.8236722229  0.0715616918 -1.3857808111
#>  [66] -2.1639369120  0.6456528830  0.4191619146  1.3497939535  0.1910514929
#>  [71]  0.1208084919  1.1410898907 -2.2512851976  0.4724749199  0.5406898973
#>  [76]  0.2532050841  0.6781773409  1.5930363601  0.6196782911  0.1799630402
#>  [81] -1.4868659312 -1.5663243894  1.6588813968 -0.4051852902 -0.4799804012
#>  [86]  0.8311851349 -0.6040946569  1.2374930398 -0.4536413104 -0.9572455387
#>  [91] -1.4061602817 -1.2849108996 -0.6768449790  1.3110451395 -1.3574792727
#>  [96] -1.9999104669 -0.2800871704 -0.0361899557 -0.2862667112 -0.1846429500
#> [101]  0.9637178832 -1.2638656695 -0.4217000731 -0.0965898022 -1.4741532671
#> [106]  0.6898840458  1.1119456174  1.2922687379  0.9774092675  0.2516724271
#> [111] -0.7082200987 -0.3377183713 -0.1166907466 -0.3731545408  0.8209343927
#> [116]  1.8524082577 -0.3372680531  1.0610469204 -0.7827609120 -1.0360655304
#> [121]  0.8001476010  0.6341864978  1.8714130557 -0.0724686715  0.4377708385
#> [126] -0.1089411775 -1.1099739945  1.1758782762 -1.7638201727 -0.1067037610
#> [131]  0.3130573937 -1.9414695478  1.3163486012 -1.4969349345  1.1594643621
#> [136] -0.3357445190  1.1437658039 -1.1889937766  1.5208457176 -0.7943066334
#> [141] -1.6062685960 -0.8934062775 -1.5110273758 -0.6991549357 -0.8852334540
#> [146]  0.3661392331 -0.7139237314  0.6797679920  0.4207168615  0.8785929692
#> [151]  1.6023486737  0.3168190038 -0.4660796030  0.1623115575  0.0458698622
#> [156]  0.3410648432  0.0422072447  1.7116945134 -0.1696931130 -0.4686990773
#> [161] -0.0913296654  0.4082637446  0.6131126668 -0.6327863750  1.1927622283
#> [166]  0.3569965516 -0.2703903907  1.6837715932  0.1219211453 -1.1685787807
#> [171]  1.8371146990  0.6328632682  0.9959931647 -0.0652626906 -1.6575475349
#> [176] -0.1550597630  0.7661258472 -0.8890052875 -0.0338127778  1.5019798411
#> [181] -0.1754524008 -1.1618320827 -1.0313960673  0.0164578763 -0.2520328454
#> [186] -1.7396534913 -1.0397239721 -0.2950816591  0.2658185978 -0.1923111162
#> [191] -0.2080030808  1.7381073902 -0.5785095967 -1.4028352650 -0.7852249307
#> [196]  0.4194999166 -0.1183565184 -0.2545918184  1.3265273454  0.3692022164
#> [201]  1.6374814370  1.8409560848 -0.6344756466  0.2232390445 -0.7592299434
#> [206] -0.7347417518  0.2432430148  1.0863976956  0.4304413158  0.4263177017
#> [211] -2.2518706680  0.4560333037 -0.1797650557 -0.1470162795 -0.4940973327
#> [216]  1.7054960786  0.2938839093 -2.3154917268 -0.5463244563 -0.8951019167
#> [221]  0.9925999359 -0.2229051620  0.1229486602 -0.0612021985 -0.7967121511
#> [226] -0.2774226634 -0.8154646516 -0.7225450847  1.3824029054  0.5591371151
#> [231]  0.3934040002 -0.5616926964 -0.3519474411  0.3625570263 -0.2810536359
#> [236]  0.7919986408 -0.3889989582  1.0600081474  0.8566948182 -1.8517422520
#> [241] -2.0217800693  1.0105044669 -1.1883486059  0.9154018423 -0.6114377619
#> [246]  0.2913931239  1.1566823849  0.4805564584  1.0420001287 -3.1745989197
#> [251]  0.4641937103 -0.7548435060  0.2604226491 -0.8002368586  0.4412491305
#> [256]  1.4341686332 -0.2824018710  0.6702466383  0.3754499376 -0.8073116407
#> [261]  0.4478667638  0.4980312747 -0.4444347289  1.3353999808 -0.6737937699
#> [266]  0.1347512101 -0.5149546555 -0.5568463719  1.3109581072 -0.8662185201
#> [271] -1.0713216894 -1.5778771157 -0.8729604050  0.1318923833 -0.4385695989
#> [276] -0.0767818506  0.3084377315  0.2481228095 -0.6929098033 -0.3803065033
#> [281]  1.3132590580  0.6993961465  1.3335801911  0.0625625597  0.6819756719
#> [286]  0.1462055864 -1.4727271204  1.0906691369  0.9584852901 -0.6090304149
#> [291] -0.2616325947  1.2313229104  0.6130752839 -0.8875159196  0.5207990608
#> [296]  0.4595022815 -0.7786385448 -1.1838812319  1.8253856849  0.3458337835
#> [301] -0.4427246561 -0.1432139494 -0.4651685352  0.6047575802 -0.8699157215
#> [306] -1.7426052155 -0.4159645093  0.6049663945 -1.0448556503 -0.0722954162
#> [311] -0.5888842112  0.2371528666  1.2107377605 -0.3984704353 -0.4971356724
#> [316]  0.7525263117  0.2374861472  0.0485866424  1.5430891919  0.5963358156
#> [321] -0.4165254171  0.3864681432 -0.5474483920 -1.0479711477 -1.4936992670
#> [326]  0.0521424248  0.6021738509  1.7554016312  0.7094266338  1.1347868613
#> [331] -0.0841635588  0.8331590995  2.1544925682  1.6445444703 -1.0704845256
#> [336]  0.0422949184  0.9158362154 -0.8652747275 -0.4634570399  0.8820311160
#> [341] -1.2290194130 -0.5469180840  0.0115559178  1.2749988739  1.3196411685
#> [346] -0.0941379407  0.0821894630  0.1585340454 -0.2809634145  0.5605975850
#> [351]  0.3210435388 -0.5464417046  0.0548117275  1.2785517452 -0.5813647525
#> [356] -0.9268450187 -0.8100221758 -1.6228924603 -0.6845659265  0.2383717814
#> [361]  0.9373665455 -1.2010774371  0.2778870068  0.6983516870  0.3360968725
#> [366]  1.2321538332 -0.4502233484 -0.0765423638  0.5728145982  0.5933717589
#> [371] -2.7595058681 -0.9067932563  0.2513154764 -1.9073760316 -0.6179707379
#> [376] -0.1399576256 -0.3481875728  3.3092854483 -0.3109331235  1.5769403726
#> [381] -2.6781075222 -0.4429096259 -0.5876180661  0.4999842463  0.3313270198
#> [386] -0.1651416824 -0.0404624874 -1.4177148596 -0.9429902703 -0.9663561862
#> [391] -0.6466191658 -2.2535063005  0.4563135780  0.1453231304 -0.1182882332
#> [396]  1.6290602024  0.3146280392 -0.3493814754  1.0416358515  0.0136480171
#> [401]  1.7026248170 -0.0356641510  1.0414162785  0.5572875838  0.3550610267
#> [406]  0.2089424426  1.4368057614  0.2836747953  0.6772658681  1.0065016707
#> [411]  1.2001797239  0.5316990211  0.6336526750 -0.0190973637 -0.4963304733
#> [416] -1.4052713714 -0.4877801835 -0.9729185834  0.0800046880  1.9984809435
#> [421]  0.0146878993 -0.0014265194  0.9781840852  0.2784228533 -0.7805456237
#> [426]  0.8431370787 -0.9847933905 -0.8886963475  1.6803385316  1.0750066150
#> [431] -0.0393142744  0.4961341049  0.5191451935 -0.4573708258  0.6990303540
#> [436] -1.0057233670  2.1745715012  1.3823092213  1.6431382695 -0.6682482269
#> [441]  0.3092682477 -0.1228064748  0.9013385029  0.6395657029  1.0544417016
#> [446] -0.3859865656  0.3428001042 -0.0580379891  1.1240987437 -1.9467612774
#> [451]  0.3093001178 -0.2042447739 -0.7712826657 -0.5557474232  0.2159453336
#> [456]  0.1312795882 -1.5114788550 -0.8743767375 -0.8762197629 -0.9952456694
#> [461] -1.1590765928 -1.3025718628  1.1807930021 -0.4665046434 -0.2828278537
#> [466] -0.8862018395  0.2627760037 -0.2246585728 -0.7350270917  2.2330846670
#> [471] -0.1680653380  0.2272482221 -0.4662419908 -0.3983863095 -0.8770483090
#> [476] -0.3627669389 -1.2516292128  0.8971122637 -0.7173723569 -0.1069220469
#> [481]  0.1134832246  0.3757775355 -0.2128705850 -0.3102625447 -0.1662801672
#> [486] -1.0328202284 -0.6530835138  0.8961528576  0.0541645528 -0.9043953545
#> [491]  0.9533184156 -1.0979696605  1.1275981322 -0.7743775123 -0.8219478749
#> [496]  1.4861078517  0.7376918609  0.3628190010 -0.9383643181  0.3175034734
#> [501]  1.3442690156 -1.8287202394 -0.0867610000  1.8935600524 -0.6001433545
#> [506]  0.3359892081 -0.4675070243  1.2196363492  0.6361736837  0.5308643245
#> [511] -0.2883264347  0.9624697278 -1.4780914615 -0.8452842468 -1.2242792315
#> [516]  0.6984536826 -0.1638332534  3.1504729615 -1.0518676907 -0.9852678720
#> [521]  1.7339047855 -1.8750237951  0.4999242945 -1.0247546509 -1.4427773430
#> [526]  0.7390501284 -0.8521673979 -1.8048482025 -0.3303034285  0.7739312929
#> [531] -0.7394480434  0.9835059653  1.9907888113  1.4970706001  0.5331882492
#> [536]  0.7105372605  0.5615316312
```
