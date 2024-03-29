#' glmmTMB wrapper function
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits a glmmTMB model. A glmmTMB model fit is
#'   returned.
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
#' @return A fitted glmmTMB model object stored in a list. The fit time in
#'   seconds in also included in this list
glmmtmb_wrapper_fun <- function(
    participant,
    visit_num,
    base_bcva,
    strata,
    trt,
    bcva_change,
    covar_type) {
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

  if (covar_type == "csh") {
    fit_time <- microbenchmark::microbenchmark(
      fit <- glmmTMB::glmmTMB(
        formula = bcva_change ~ base_bcva + strata + trt * visit_num +
          cs(visit_num + 0 | participant),
        dispformula = ~0,
        data = df
      ),
      times = 1L
    )
  } else if (covar_type == "us") {
    fit_time <- microbenchmark::microbenchmark(
      fit <- glmmTMB::glmmTMB(
        formula = bcva_change ~ base_bcva + strata + trt * visit_num +
          us(visit_num + 0 | participant),
        dispformula = ~0,
        data = df
      ),
      times = 1L
    )
  } else if (covar_type == "toeph") {
    fit_time <- microbenchmark::microbenchmark(
      fit <- glmmTMB::glmmTMB(
        formula = bcva_change ~ base_bcva + strata + trt * visit_num +
          toep(visit_num + 0 | participant),
        dispformula = ~0,
        data = df
      ),
      times = 1L
    )
  } else {
    stop("This covariance matrix is not supported by this wrapper function.")
  }

  return(list(
    fit = fit,
    fit_time = fit_time$time / 1e9 # NOTE: time in seconds
  ))
}
