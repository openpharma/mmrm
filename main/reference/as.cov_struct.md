# Coerce into a Covariance Structure Definition

**\[stable\]**

## Usage

``` r
as.cov_struct(x, ...)

# S3 method for class 'formula'
as.cov_struct(x, warn_partial = TRUE, ...)
```

## Arguments

- x:

  an object from which to derive a covariance structure. See object
  specific sections for details.

- ...:

  additional arguments unused.

- warn_partial:

  (`flag`)  
  whether to emit a warning when parts of the formula are disregarded.

## Value

A
[`cov_struct()`](https://openpharma.github.io/mmrm/reference/cov_struct.md)
object.

## Details

A covariance structure can be parsed from a model definition formula or
call. Generally, covariance structures defined using non-standard
evaluation take the following form:

    type( (visit, )* visit | (group /)? subject )

For example, formulas may include terms such as

    us(time | subject)
    cp(time | group / subject)
    sp_exp(coord1, coord2 | group / subject)

Note that only `sp_exp` (spatial) covariance structures may provide
multiple coordinates, which identify the Euclidean distance between the
time points.

## Methods (by class)

- `as.cov_struct(formula)`: When provided a formula, any specialized
  functions are assumed to be covariance structure definitions and must
  follow the form:

      y ~ xs + type( (visit, )* visit | (group /)? subject )

  Any component on the right hand side of a formula is considered when
  searching for a covariance definition.

## See also

Other covariance types:
[`cov_struct()`](https://openpharma.github.io/mmrm/reference/cov_struct.md),
[`covariance_types`](https://openpharma.github.io/mmrm/reference/covariance_types.md)

## Examples

``` r
# provide a covariance structure as a right-sided formula
as.cov_struct(~ csh(visit | group / subject))
#> <covariance structure>
#> heterogeneous compound symmetry:
#> 
#>   visit | group / subject
#>  

# when part of a full formula, suppress warnings using `warn_partial = FALSE`
as.cov_struct(y ~ x + csh(visit | group / subject), warn_partial = FALSE)
#> <covariance structure>
#> heterogeneous compound symmetry:
#> 
#>   visit | group / subject
#>  
```
