# Get Prediction

Get predictions with given `data`, `theta`, `beta`, `beta_vcov`.

## Usage

``` r
h_get_prediction(tmb_data, theta, beta, beta_vcov)
```

## Arguments

- tmb_data:

  (`mmrm_tmb_data`)  
  object.

- theta:

  (`numeric`)  
  theta value.

- beta:

  (`numeric`)  
  beta value.

- beta_vcov:

  (`matrix`)  
  beta_vcov matrix.

## Value

List with:

- `prediction`: Matrix with columns `fit`, `conf_var`, and `var`.

- `covariance`: List with subject specific covariance matrices.

- `index`: List of zero-based subject indices.

## Details

See `predict` function in `predict.cpp` which is called internally.
