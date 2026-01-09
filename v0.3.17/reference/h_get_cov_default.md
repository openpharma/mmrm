# Obtain Default Covariance Method

Obtain the default covariance method depending on the degrees of freedom
method used.

## Usage

``` r
h_get_cov_default(
  method = c("Satterthwaite", "Kenward-Roger", "Residual", "Between-Within")
)
```

## Arguments

- method:

  (`string`)  
  degrees of freedom method.

## Value

String of the default covariance method.

## Details

The default covariance method is different for different degrees of
freedom method. For "Satterthwaite" or "Between-Within", "Asymptotic" is
returned. For "Kenward-Roger" only, "Kenward-Roger" is returned. For
"Residual" only, "Empirical" is returned.
