# Obtain Optimizer Function with Character

Obtain the optimizer function through the character provided.

## Usage

``` r
h_optimizer_fun(optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb"))
```

## Arguments

- optimizer:

  (`character`)  
  vector of optimizers.

## Value

A (`list`)  
of optimizer functions generated from
[`h_partial_fun_args()`](https://openpharma.github.io/mmrm/reference/h_partial_fun_args.md).
