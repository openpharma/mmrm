# Refit an `mmrm` Model Using a New Dataset

Extract the `call`
[component](https://openpharma.github.io/mmrm/reference/component.md) of
`fit` and evaluate it in a new
[environment](https://rdrr.io/r/base/environment.html) that contains the
new `data`.

## Usage

``` r
h_refit_mmrm(fit, data)
```

## Arguments

- fit:

  (`mmrm`)  
  an `mmrm` object to be refit.

- data:

  (`data frame`)  
  a data frame upon which `fit` is to be refit.

## Value

An `mmrm` object with the same terms as `fit` but based on `data`.

## Details

This works as follows:

1.  A new environment is created whose parent is
    `environment(fit$formula_parts$full_formula)`.

2.  A name is generated using
    [`h_generate_new_name()`](https://openpharma.github.io/mmrm/reference/h_generate_new_name.md),
    and `data` is bound to the new environment using this new name.

3.  The [`call`](https://rdrr.io/r/base/call.html) component of `fit` is
    extracted and its `data` argument is changed to the new name.

4.  The modified call is evaluated in the new environment.
