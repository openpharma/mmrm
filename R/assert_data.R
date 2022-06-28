#' @include utils.R
NULL

#' Assertions for Datasets
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide assertion functions to check the input dataset.
#'
#' @param vars (`list`)\cr variables to use.
#' @param data (`data.frame`)\cr data to use.
#'
#' @return Nothing, only fails with an error if assertion does not hold.
#'
#' @name assert_data
NULL

#' @describeIn assert_data asserts single record per patient and visit.
#' @export
h_assert_one_rec_pt_visit <- function(vars, data) {
  assert_list(vars)
  assert_string(vars$visit)
  assert_string(vars$id)
  assert_data_frame(data)

  visit_id_combinations <- interaction(data[[vars$visit]], data[[vars$id]])
  grouped_data <- split(data, f = visit_id_combinations)
  n_per_group <- vapply(grouped_data, nrow, integer(1))

  if (any(n_per_group > 1)) {
    dupl_group <- which(n_per_group > 1)
    n_dupl <- length(dupl_group)
    dupl_rows <- unique(do.call(rbind, grouped_data[names(dupl_group)]))
    dupl_strings <- paste(
      vars$id, "=", dupl_rows[[vars$id]], "with",
      vars$visit, "=", dupl_rows[[vars$visit]]
    )
    stop(paste(
      "There are", n_dupl, "subjects with more than one record per visit:",
      toString(dupl_strings)
    ))
  }
}

#' @describeIn assert_data assert numeric response variable.
#' @export
h_assert_rsp_var <- function(vars, data) {
  assert_list(vars)
  assert_string(vars$response)
  assert_data_frame(data)

  response_values <- data[[vars$response]]
  assert_numeric(response_values)
}

#' @describeIn assert_data assert factor visit variable.
#' @export
h_assert_visit_var <- function(vars, data) {
  assert_list(vars)
  assert_string(vars$visit)
  assert_data_frame(data)

  visit_values <- data[[vars$visit]]
  assert_factor(visit_values)
}

#' @describeIn assert_data high-level assertion function to check the dataset.
#' @export
assert_data <- function(vars, data) {
  assert_list(vars)
  assert_string(vars$arm, null.ok = TRUE)
  assert_string(vars$visit)
  assert_string(vars$response)
  assert_data_frame(data)

  # First subset data to observations with complete regressors.
  regressor_vars <- c(vars$arm, vars$visit, h_get_covariate_parts(vars$covariates))
  has_complete_regressors <- stats::complete.cases(data[, regressor_vars, drop = FALSE])
  data_complete_regressors <- droplevels(data[has_complete_regressors, , drop = FALSE])

  h_assert_one_rec_pt_visit(vars, data_complete_regressors)
  h_assert_rsp_var(vars, data_complete_regressors)
  h_assert_visit_var(vars, data_complete_regressors)

  # Second only look at complete data.
  has_complete_response <- stats::complete.cases(data_complete_regressors[, vars$response, drop = FALSE])
  data_complete <- droplevels(data_complete_regressors[has_complete_response, , drop = FALSE])

  if (!is.null(vars$arm)) {
    assert_factor(data_complete_regressors[[vars$arm]], min.levels = 2L)
    assert_factor(
      data_complete[[vars$arm]],
      levels = levels(data_complete_regressors[[vars$arm]])
    )
    assert_true(all(table(data_complete[[vars$arm]]) > 5))
  } else {
    assert_data_frame(data_complete, min.rows = 5L)
  }
}
