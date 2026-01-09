# Obtain Optimizer according to Optimizer String Value

This function creates optimizer functions with arguments.

## Usage

``` r
h_get_optimizers(
  optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb"),
  optimizer_fun = h_optimizer_fun(optimizer),
  optimizer_args = list(),
  optimizer_control = list()
)
```

## Arguments

- optimizer:

  (`character`)  
  names of built-in optimizers to try, subset of "L-BFGS-B", "BFGS",
  "CG" and "nlminb".

- optimizer_fun:

  (`function` or `list` of `function`)  
  alternatively to `optimizer`, an optimizer function or a list of
  optimizer functions can be passed directly here.

- optimizer_args:

  (`list`)  
  additional arguments for `optimizer_fun`.

- optimizer_control:

  (`list`)  
  passed to argument `control` in `optimizer_fun`.

## Value

Named `list` of optimizers created by
[`h_partial_fun_args()`](https://openpharma.github.io/mmrm/reference/h_partial_fun_args.md).

## Details

If you want to use only the built-in optimizers:

- `optimizer` is a shortcut to create a list of built-in optimizer
  functions passed to `optimizer_fun`.

- Allowed are "L-BFGS-B", "BFGS", "CG" (using
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) with
  corresponding method) and "nlminb" (using
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html)).

- Other arguments should go into `optimizer_args`.

If you want to use your own optimizer function:

- Make sure that there are three arguments: parameter (start value),
  objective function and gradient function are sequentially in the
  function arguments.

- If there are other named arguments in front of these, make sure they
  are correctly specified through `optimizer_args`.

- If the hessian can be used, please make sure its argument name is
  `hessian` and please add attribute `use_hessian = TRUE` to the
  function, using `attr(fun, "use_hessian) <- TRUE`.
