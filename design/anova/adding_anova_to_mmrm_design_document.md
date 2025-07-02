# Adding anova() to mmrm
Nik Krieger

# Background

A current drawback of `mmrm` is that it does not support
`stats::anova()` (i.e., `anova.mmrm()` does not exist). This is
discussed here: <https://github.com/openpharma/mmrm/issues/164>

## Key points from the GitHub issue

The title of the issue is centered on incorporating the Likelihood Ratio
Test for `mmrm` fits: “\[feature request\] add likelihood ratio test.”

There is consensus that this goal of obtaining LRT results would be
available via a new `anova()` method for `mmrm` model fits.

### Models should be compared to check if they are nested:

- Nested sets of covariates. See:
  <https://github.com/openpharma/mmrm/issues/164#issuecomment-1446364890>
- “Nested” covariance structures (e.g., unstructured + any structured;
  AR1 being a special case of Toeplitz). See:
  <https://github.com/openpharma/mmrm/issues/164#issuecomment-1446627674>.
- **Not both** of the above, as this would be too complicated.

### Accommodate comparison of models that used different data sets

It’s possible that the larger model was fit with an augmented data set
(<https://github.com/openpharma/mmrm/issues/164#issuecomment-1446839199>).
In this case, it would be advantageous for the function to refit the
model
(<https://github.com/openpharma/mmrm/issues/164#issuecomment-1447419569>).

## Additional considerations

### Providing only one fit to `anova.mmrm()`

`?stats::anova` asserts that if only one model fit is provided, the
function will calculate the significance of model terms. The
`anova.mmrm()` should follow this lead and provide these results
accordingly.

### Following the lead of `anova()` methods in `nlme` and `lme4`

In order that `anova.mmrm()` may be comparable to `anova()` methods in
the `nlme` and `lme4` packages, it shall contain the following
arguments:

- `test` argument to opt in/out of obtaining LRT results (see
  `?nlme::anova.lme`)
- `refit` arguments to allow for refitting the models (see
  `?lme4::anova.merMod`)

# Prerequisite: augment `logLik.mmrm_tmb()`

This function needs to return more data in the `attributes()` of the
returned value:

- `nall`, `nobs`, and `df`
- Add `"logLik"` to the `class()` of the returned value

# Designing `anova.mmrm()`

## Function specification

### Usage

``` r
## S3 method for class 'mmrm'
anova(object, ..., test = TRUE, refit = FALSE)
```

### Arguments

|  |  |
|----|----|
| `object` | An object inheriting from class `mmrm`: a fitted mixed model with repeated measures. |
| `...` | Other optional fitted model objects inheriting from class `mmrm`. |
| `test` | Logical indicating whether likelihood ratio tests should be used to compare the fitted models represented by `object` and the object(s) in `...`. Defaults to `TRUE`. |
| `refit` | Logical indicating if the models should be refitted before comparing. Defaults to `FALSE`. |

## Function outline

### Plain language

#### Special case: supplying only one model fit

If the user provides only one model fit to `anova.mmrm()`, the function
will calculate and return the significance of the model terms.

#### Comparing multiple model fits

If the user provides more than one model fit, a `tibble` of results is
produced. This `tibble` will have a row for each model, and the columns
will contain identifying information as well as standard diagnostics
(e.g., AIC, log likelihood, etc.).

If the user supplied `test = TRUE`, the model fits are subject to
further tests to ensure they are nestable. If the data sets for the fits
are not the same and the user supplied `refit = TRUE`, the data sets are
harmonized and the models are refitted using the harmonized data set.
The pairs of models then undergo likelihood ratio testing, and the
results are augmented into the `tibble` of results.

### Pseudocode

``` r
anova.mmrm <- function (object, ..., test = TRUE, refit = FALSE) {
  
  # If there are objects supplied to "..."
  if (...length()) {
    
    # Gather all objects into one list
    fits <- list(object, ...)
    
    # Ensure all the objects are mmrm fits
    validate_mmrm_objects(fits)
    
    # Calculate the standard diagnostics no matter what
    results <-
      tibble(
        call = lapply(fits, mmrm::component, "call"),
        Model = row_number(),
        df = vapply(fits, get_df, integer(1)), 
        AIC = vapply(fits, AIC, numeric(1)),
        BIC = vapply(fits, BIC, numeric(1)),
        logLik = vapply(fits, logLik, numeric(1))
      )
    
    # If the user has requested the likelihood ratio test...
    if (test) {
      
      # Ensure all fits can be nested
      ensure_same_formula_for_fixed_effects(fits)
      ensure_same_time_group_subjectID_vars(fits)
      ensure_nestable_covariance_structures(fits)
      
      # Handle differences in underlying data sets
      data_sets_are_same <- compare_data_underlying_model_fits(fits)
      if (!data_sets_are_same) {
        if (refit) {
          # Create one harmonized data set from all fits' underlying data
          # Errors out if not possible
          harmonized_data <- harmonize_underlying_data(fits)
          
          # Refit all models using harmonized data
          fits <- lapply(fits, refit_mmrm, harmonized_data)
        } else {
          stop("Data underlying mmrm fits are not all the same and refit = FALSE")
        }
      }
      
      
      results <- results |>
        mutate(
          # Label for the pair being compared (e.g., "3 vs 4")
          test = c(NA, paste0(head(Model, -1), " vs ", tail(Model, -1))),
          
          # Calculate the likelihood ratio between the pairs using base::diff()
          likelihood_ratio = c(NA, diff(logLik)),
          
          # Calculate the p-value using the ratios and differences in df
          p_value = LR_pvalues(likelihood_ratio, c(NA, diff(df)))
        )
    }
    
  } else { 
    
    # If only one model fit was provided, calculate the significance of model
    # terms, which is what ?stats::anova promises
    out <- calculate_significance_of_model_terms(object)
  }
  
  out
}
```

# Examples

Insert examples here.
