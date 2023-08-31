#' Register `mmrm` For Use With `tidymodels`
#'
#' @inheritParams base::requireNamespace
#' @return A logical value indicating whether registration was successful.
#'
#' @details We can use `parsnip::show_model_info("linear_reg")` to check the
#'   registration with `parsnip` and thus the wider `tidymodels` ecosystem.
#'
#' @keywords internal
parsnip_add_mmrm <- function(quietly = FALSE) {
  if (!requireNamespace("parsnip", quietly = quietly)) {
    return(FALSE)
  }

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
      allow_sparse_x = TRUE
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

  parsnip::set_pred(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression",
    type = "numeric",
    value = parsnip::pred_value_template(
      # This is boilerplate.
      func = c(fun = "predict"),
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "mmrm",
    mode = "regression",
    # This type allows to pass arguments via `opts` to `parsnip::predict.model_fit`.
    type = "raw",
    value = parsnip::pred_value_template(
      # This is boilerplate.
      func = c(fun = "predict"),
      object = quote(object$fit),
      newdata = quote(new_data)
      # We don't specify additional argument defaults here since otherwise
      # the user is not able to change them (they will be fixed).
    )
  )

  TRUE
}
