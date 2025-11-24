# Covariance Types

**\[stable\]**

## Usage

``` r
cov_types(
  form = c("name", "abbr", "habbr"),
  filter = c("heterogeneous", "spatial")
)
```

## Arguments

- form:

  (`character`)  
  covariance structure type name form. One or more of `"name"`, `"abbr"`
  (abbreviation), or `"habbr"` (heterogeneous abbreviation).

- filter:

  (`character`)  
  covariance structure type filter. One or more of `"heterogeneous"` or
  `"spatial"`.

## Value

A character vector of accepted covariance structure type names and
abbreviations.

## Note

The **ante-dependence** covariance structure in this package refers to
homogeneous ante-dependence, while the ante-dependence covariance
structure from SAS `PROC MIXED` refers to heterogeneous ante-dependence
and the homogeneous version is not available in SAS.

For all non-spatial covariance structures, the time variable must be
coded as a factor.

### Spatial Covariance structures:

|  |  |  |  |
|----|----|----|----|
| **Structure** | **Description** | **Parameters** | **\\(i, j)\\ element** |
| sp_exp | spatial exponential | \\2\\ | \\\sigma^{2}\rho^{-d\_{ij}}\\ |

where \\d\_{ij}\\ denotes the Euclidean distance between time points
\\i\\ and \\j\\.

## Abbreviations for Covariance Structures

### Common Covariance Structures:

|  |  |  |  |
|----|----|----|----|
| **Structure** | **Description** | **Parameters** | **\\(i, j)\\ element** |
| ad | Ante-dependence | \\m\\ | \\\sigma^{2}\prod\_{k=i}^{j-1}\rho\_{k}\\ |
| adh | Heterogeneous ante-dependence | \\2m-1\\ | \\\sigma\_{i}\sigma\_{j}\prod\_{k=i}^{j-1}\rho\_{k}\\ |
| ar1 | First-order auto-regressive | \\2\\ | \\\sigma^{2}\rho^{\left \vert {i-j} \right \vert}\\ |
| ar1h | Heterogeneous first-order auto-regressive | \\m+1\\ | \\\sigma\_{i}\sigma\_{j}\rho^{\left \vert {i-j} \right \vert}\\ |
| cs | Compound symmetry | \\2\\ | \\\sigma^{2}\left\[ \rho I(i \neq j)+I(i=j) \right\]\\ |
| csh | Heterogeneous compound symmetry | \\m+1\\ | \\\sigma\_{i}\sigma\_{j}\left\[ \rho I(i \neq j)+I(i=j) \right\]\\ |
| toep | Toeplitz | \\m\\ | \\\sigma\_{\left \vert {i-j} \right \vert +1}\\ |
| toeph | Heterogeneous Toeplitz | \\2m-1\\ | \\\sigma\_{i}\sigma\_{j}\rho\_{\left \vert {i-j} \right \vert}\\ |
| us | Unstructured | \\m(m+1)/2\\ | \\\sigma\_{ij}\\ |

where \\i\\ and \\j\\ denote \\i\\-th and \\j\\-th time points,
respectively, out of total \\m\\ time points, \\1 \leq i, j \leq m\\.

## See also

Other covariance types:
[`as.cov_struct()`](https://openpharma.github.io/mmrm/reference/as.cov_struct.md),
[`cov_struct()`](https://openpharma.github.io/mmrm/reference/cov_struct.md)
