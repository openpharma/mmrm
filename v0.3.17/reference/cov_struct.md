# Define a Covariance Structure

**\[stable\]**

## Usage

``` r
cov_struct(
  type = cov_types(),
  visits,
  subject,
  group = character(),
  heterogeneous = FALSE
)
```

## Arguments

- type:

  (`string`)  
  the name of the covariance structure type to use. For available
  options, see
  [`cov_types()`](https://openpharma.github.io/mmrm/reference/covariance_types.md).
  If a type abbreviation is used that implies heterogeneity (e.g. `cph`)
  and no value is provided to `heterogeneous`, then the heterogeneity is
  derived from the type name.

- visits:

  (`character`)  
  a vector of variable names to use for the longitudinal terms of the
  covariance structure. Multiple terms are only permitted for the
  `"spatial"` covariance type.

- subject:

  (`string`)  
  the name of the variable that encodes a subject identifier.

- group:

  (`string`)  
  optionally, the name of the variable that encodes a grouping variable
  for subjects.

- heterogeneous:

  (`flag`)  

## Value

A `cov_struct` object.

## See also

Other covariance types:
[`as.cov_struct()`](https://openpharma.github.io/mmrm/reference/as.cov_struct.md),
[`covariance_types`](https://openpharma.github.io/mmrm/reference/covariance_types.md)

## Examples

``` r
cov_struct("csh", "AVISITN", "USUBJID")
#> <covariance structure>
#> heterogeneous compound symmetry:
#> 
#>   AVISITN | USUBJID
#>  
cov_struct("spatial", c("VISITA", "VISITB"), group = "GRP", subject = "SBJ")
#> <covariance structure>
#> spatial exponential:
#> 
#>   VISITA, VISITB | GRP / SBJ
#>  
```
