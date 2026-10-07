# Obtain Kenward-Roger Adjustment Components

Obtains the components needed downstream for the computation of
Kenward-Roger degrees of freedom. Used in
[`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md) fitting
if method is "Kenward-Roger".

## Usage

``` r
h_get_kr_comp(tmb_data, theta, linear = FALSE, w = NULL)
```

## Arguments

- tmb_data:

  (`mmrm_tmb_data`)\
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- theta:

  (`numeric`)\
  theta estimate.

- linear:

  (`flag`)\
  whether to omit second derivatives and the R component.

- w:

  (`matrix` or `NULL`)\
  covariance of the covariance parameters. Supply with `linear = TRUE`
  to contract Q without constructing its blocks.

## Value

Named list with elements:

- `P`: `matrix` of \\P\\ component.

- `Q`: `matrix` of \\Q\\ component.

- `R`: `matrix` of \\R\\ component, or `NULL` when `linear = TRUE`.

- `S_Q`: contracted Q sum when `w` is supplied; `Q` and `R` are then
  `NULL`.

## Details

the function returns a named list, \\P\\, \\Q\\ and \\R\\, which
corresponds to the paper in 1997. The matrices are stacked in columns so
that \\P\\, \\Q\\ and \\R\\ has the same column number(number of beta
parameters). The number of rows, is dependent on the total number of
theta and number of groups, if the fit is a grouped mmrm. For \\P\\
matrix, it is stacked sequentially. For \\Q\\ and \\R\\ matrix, it is
stacked so that the \\Q\_{ij}\\ and \\R\_{ij}\\ is stacked from \\j\\
then to \\i\\, i.e. \\R\_{i1}\\, \\R\_{i2}\\, etc. \\Q\\ and \\R\\ only
contains intra-group results and inter-group results should be all zero
matrices so they are not stacked in the result.

Supplying `w` for linear KR instead retains `P` and contracts Q into the
single matrix `S_Q`, without storing Q blocks, see the section
"Contracted linear covariance adjustment" in
[`vignette("kenward", package = "mmrm")`](https://openpharma.github.io/mmrm/articles/kenward.md).
The symmetric part of `w` is used, and non-finite entries in `w` give a
non-finite `S_Q`.
[`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md) always
supplies `w` for linear KR. Calling with `linear = TRUE` but without `w`
gives the pairwise `P` and `Q` blocks, which tests use as an independent
reference.
