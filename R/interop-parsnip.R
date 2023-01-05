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

  parsnip::set_dependency(
    pkg = "multilevelmod",
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
      data = c(formula = "model", data = "data"),
      func = c(pkg = "mmrm", fun = "mmrm"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(result, object) {
        warning("predicting with mmrm is not yet implemented!")
        1:3
      },
      func = c(fun = "predict"),
      args = list()
    )
  )

  TRUE
}