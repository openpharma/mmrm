################################################################################
## Method Wrapper Functions
################################################################################

#' mmrm wrapper function
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits a mmrm model. A mmrm model fit is returned.
#'
#' @param participant A factor vector of participant IDs.
#' @param time A factor vector of time points.
#' @param y A numeric vector of outcomes.
#' @param trt A binary vector of treatment group indicators.
#' @param base_cov A numeric vector of baseline covariate values.
#'
#' @return A fitted mmrm model object.
mmrm_wrapper_fun <- function(
  participant,
  time,
  y,
  trt,
  base_cov
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt,
    "base_cov" = base_cov
  )

  fit <- mmrm::mmrm(
    formula = y ~ base_cov + trt*time + csh(time | participant), data = df
  )

  return(fit)
}

#' glmmTMB wrapper function
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits a glmmTMB model. A glmmTMB model fit is returned.
#'
#' @param participant A factor vector of participant IDs.
#' @param time A factor vector of time points.
#' @param y A numeric vector of outcomes.
#' @param trt A binary vector of treatment group indicators.
#' @param base_cov A numeric vector of baseline covariate values.
#'
#' @return A fitted glmmTMB model object.
glmmTMB_wrapper_fun <- function(
  participant,
  time,
  y,
  trt,
  base_cov
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt,
    "base_cov" = base_cov
  )

  fit <- glmmTMB::glmmTMB(
    formula = y ~ base_cov + trt*time + cs(time + 0 | participant), data = df,
    dispformula = ~ 0
  )

}

#' nlme wrapper function
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits a gls model. A gls model fit is returned.
#'
#' @param participant A factor vector of participant IDs.
#' @param time A factor vector of time points.
#' @param y A numeric vector of outcomes.
#' @param trt A binary vector of treatment group indicators.
#' @param base_cov A numeric vector of baseline covariate values.
#'
#' @return A fitted gls model object.
nlme_wrapper_fun <- function(
  participant,
  time,
  y,
  trt,
  base_cov
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt,
    "base_cov" = base_cov
  )

  fit <- nlme::gls(
    y ~ base_cov + trt*time,
    correlation = nlme::corCompSymm(form = ~ 1 | participant),
    weights = nlme::varIdent(form = ~ 1 | time), data = df
  )

  return(fit)

}
