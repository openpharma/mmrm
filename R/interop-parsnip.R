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

  args <- fill_names(c(
    "reml",
    "n_cores"
  ))

  mapply(
    parsnip = names(args),
    original = args,
    parsnip::set_model_arg,
    MoreArgs = list(
      model = "linear_reg",
      eng = "mmrm",
      func = list(pkg = "mmrm", fun = "mmrm"),
      has_submodel = FALSE
    )
  )

  parsnip::set_fit(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      data = c(formula = "formula", data = "data", weights = "weights"),
      func = c(pkg = "mmrm", fun = "mmrm"),
      defaults = list()
    )
  )

  # awaiting `predict()` implementation in #189 for `parsnip::set_pred()`

  TRUE
}
