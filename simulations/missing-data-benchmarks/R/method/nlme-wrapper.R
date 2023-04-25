#' nlme wrapper function
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits a gls model. A gls model fit is returned.
#'
#' @param participant A numeric vector of participant IDs.
#' @param visit_num A numeric vector correpsonding to the visit number.
#' @param base_bcva A numeric corresponding to the participant's baseline BCVA
#'   value.
#' @param strata A factor vector representing the participant's strata value.
#' @param trt A binary vector of treatment group indicators.
#' @param bcva_change A numeric vector indicating the change in BCVA score since
#'   baseline.
#' @param covar_type A character indicating the type of covariance matrix to
#'   model the repeated measures with.
#'
#' @return A fitted gls model object.
nlme_wrapper_fun <- function(
  participant,
  visit_num,
  base_bcva,
  strata,
  trt,
  bcva_change,
  covar_type
) {

  ## assemble the vectors into a data.frame
  df <- assemble_df(
    participant,
    visit_num,
    base_bcva,
    strata,
    trt,
    bcva_change,
    participant_as_factor = TRUE,
    visit_num_as_factor = TRUE
  )

  # NOTE: nlme produces an error when the model fails to converge. This function
  # safely returns an error message, and allows us to check if the model
  # converged.
  safe_gls <- purrr::safely(nlme::gls)

  if (covar_type == "csh") {
    fit_time <- microbenchmark::microbenchmark(
      fit <- safe_gls(
        bcva_change ~ base_bcva + strata + trt * visit_num,
        correlation = nlme::corCompSymm(form = ~ 1 | participant),
        weights = nlme::varIdent(form = ~ 1 | visit_num), data = df,
        control = list(returnObject = TRUE)
      ),
      times = 1L
    )

  } else if (covar_type == "us") {
    fit_time <- microbenchmark::microbenchmark(
      fit <- safe_gls(
        bcva_change ~ base_bcva + strata + trt * visit_num,
        correlation = nlme::corSymm(form = ~ 1 | participant),
        weights = nlme::varIdent(form = ~ 1 | visit_num), data = df,
        control = list(returnObject = TRUE)
      ),
      times = 1L
    )

  } else {
    stop("This covariance matrix is not supported by this wrapper function.")
  }

  # extract convergence status
  converged <- is.null(fit$error)

  return(list(
    fit = fit$result,
    converged = converged,
    data = df,
    fit_time = fit_time$time / 1e9 # NOTE: time in seconds
  ))

}
