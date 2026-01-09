# Refitting MMRM with Multiple Optimizers

**\[stable\]**

## Usage

``` r
refit_multiple_optimizers(fit, ..., control = mmrm_control(...))
```

## Arguments

- fit:

  (`mmrm_fit`)  
  original model fit from
  [`fit_single_optimizer()`](https://openpharma.github.io/mmrm/reference/fit_single_optimizer.md).

- ...:

  Additional arguments passed to
  [`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md).

- control:

  (`mmrm_control`)  
  object.

## Value

The best (in terms of log likelihood) fit which converged.

## Note

For Windows, no parallel computations are currently implemented.

## Examples

``` r
fit <- fit_single_optimizer(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = rep(1, nrow(fev_data)),
  optimizer = "nlminb"
)
best_fit <- refit_multiple_optimizers(fit)
```
