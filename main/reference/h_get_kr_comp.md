# Obtain Kenward-Roger Adjustment Components

Obtains the components needed downstream for the computation of
Kenward-Roger degrees of freedom. Used in
[`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md) fitting
if method is "Kenward-Roger".

## Usage

``` r
h_get_kr_comp(tmb_data, theta)
```

## Arguments

- tmb_data:

  (`mmrm_tmb_data`)  
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- theta:

  (`numeric`)  
  theta estimate.

## Value

Named list with elements:

- `P`: `matrix` of \\P\\ component.

- `Q`: `matrix` of \\Q\\ component.

- `R`: `matrix` of \\R\\ component.

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
