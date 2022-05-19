#' Adding Labels To Variables For Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param vars (`list`)\cr variables to use.
#' @param data (`data.frame`)\cr data to use.
#' @param x (`character`)\cr an element in vars.
#' @return list of variables with labels
#'
#' @name labels
NULL

#' @describeIn labels checks if element in `vars` is not `NULL`.
#' @export
h_is_specified <- function(x, vars) {
  !is.null(vars[[x]])
}

#' @describeIn labels checks if element in vars is not NULL and exists in dataset.
#' @export
h_is_specified_and_in_data <- function(x, vars, data) {
  h_is_specified(x, vars) && all(vars[[x]] %in% names(data))
}

#' @describeIn labels gets label for each element in vars.
#' @export
h_check_and_get_label <- function(x, vars, data) {
  assert_true(h_is_specified_and_in_data(x, vars, data))
  res <- NULL
  for (v in vars[[x]]) {
    label <- attr(data[[v]], "label")
    string <- ifelse(!is.null(label), label, v)
    res <- c(res, stats::setNames(string, v))
  }
  res
}

#' @describeIn labels returns list of variables with labels.
#' @export
#' @examples
#' data <- data.frame(
#'   MYID = c(1, 1, 2, 2, 3, 3),
#'   ARM = factor(c(1, 2, 3, 1, 2, 3)),
#'   AVAL = c(2, 4, 6, 8, 10, 12),
#'   AVISIT = c("W1", "W2", "W3", "W1", "W2", "W3"),
#'   SEX = c("Female", "Female", "Female", "Male", "Male", "Male"),
#'   RACE = c("Asian", "Asian", "Asian", "White", "Asian", "White")
#' )
#' vars <- list(response = "AVAL", id = "MYID", arm = "ARM", visit = "AVISIT", covariates = c("SEX", "RACE"))
#' h_labels(vars, data)
h_labels <- function(vars,
                     data) {
  assert_list(vars)
  assert_data_frame(data)
  labels <- list()
  labels$response <- h_check_and_get_label("response", vars, data)
  labels$id <- h_check_and_get_label("id", vars, data)
  labels$visit <- h_check_and_get_label("visit", vars, data)
  if (h_is_specified("arm", vars)) {
    h_check_and_get_label("arm", vars, data)
  }
  if (h_is_specified("covariates", vars)) {
    vars$parts <- h_get_covariate_parts(vars$covariates)
    labels$parts <- h_check_and_get_label("parts", vars, data)
  }
  return(labels)
}
