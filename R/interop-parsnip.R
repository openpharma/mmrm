#' Register mmrm for use with tidymodels
#'
#' @inheritParams base::requireNamespace
#' @return A logical value indicating whether registration was successful
#'
#' @keywords internal
parsnip_add_mmrm <- function(quietly = FALSE) {
  if (!requireNamespace("parsnip", quietly = quietly)) return(FALSE)

  parsnip::set_model_engine(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression"
  )

  parsnip::set_dependency(
    pkg = "mmrm",
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression"
  )

  parsnip::set_encoding(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      data = c(formula = "formula", data = "data"),
      func = c(pkg = "mmrm", fun = "mmrm"),
      defaults = list()
    )
  )

  # awaiting `predict()` implementation in #189 for `parsnip::set_pred()`

  TRUE
}
