# Construction of Model Frame Formula and Data Inputs

Input formulas are converted from `mmrm`-style to a style compatible
with default
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) and
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
methods.

The full formula is returned so we can construct, for example, the
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) including
all columns as well as the requested subset. The full set is used to
identify rows to include in the reduced model frame.

## Usage

``` r
h_construct_model_frame_inputs(
  formula,
  data,
  include,
  include_choice = c("subject_var", "visit_var", "group_var", "response_var"),
  full
)
```

## Arguments

- formula:

  (`mmrm`)  
  fit object.

- data:

  (`data.frame`)  
  optional data frame will be passed to
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) or
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

- include:

  (`character`)  
  specification of variables to include.

- full:

  (`flag`)  
  indicator whether to return full model frame (deprecated).

## Value

A named list with two elements:

- `"formula"`: the formula including the columns requested in the
  `include=` argument.

- `"data"`: a data frame including all columns needed in the formula.
