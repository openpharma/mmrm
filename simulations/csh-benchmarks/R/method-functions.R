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
#'
#' @return A fitted mmrm model object.
mmrm_wrapper_fun <- function(
  participant,
  time,
  y,
  trt
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt
  )

  fit <- mmrm::mmrm(
    formula = y ~ trt + csh(time | participant), data = df
  )

  return(list(fit = fit))
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
#'
#' @return A fitted glmmTMB model object.
glmmtmb_wrapper_fun <- function(
  participant,
  time,
  y,
  trt
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt
  )

  ## NOTE: heterogeneous compound symmetry matrix is defined as cs() here. See
  ## https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
  ## for details.
  fit <- glmmTMB::glmmTMB(
    formula = y ~ trt + cs(time + 0 | participant), data = df,
    dispformula = ~ 0
  )

  return(list(fit = fit))

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
#'
#' @return A fitted gls model object.
nlme_wrapper_fun <- function(
  participant,
  time,
  y,
  trt
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt
  )

  fit <- nlme::gls(
    y ~ trt,
    correlation = nlme::corCompSymm(form = ~ 1 | participant),
    weights = nlme::varIdent(form = ~ 1 | time), data = df
  )

  return(list(fit = fit))

}

#' PROC MIXED Function (SAS)
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits PROC MIXED repeated measures model on this data.
#'
#' @param participant A factor vector of participant IDs.
#' @param time A factor vector of time points.
#' @param y A numeric vector of outcomes.
#' @param trt A binary vector of treatment group indicators.
#'
#' @return Estimated parameters of the repeated measures' covariance matrix.
#'   These parameters are stored in a data.frame object.
proc_mixed_fun <- function(
  participant,
  time,
  y,
  trt
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt
  )
  rownames(df) <- NULL

  ## create SAS dataset
  sasr::df2sd(df, "sas_df")

  ## specify the SAS code: only return covariance matrix estimates for
  ## repeated measures
  sas_code <- "ods output CovParms = cov_parms;
    PROC MIXED DATA = sas_df;
      CLASS trt time participant;
      MODEL y = trt;
      REPEATED time / subject=participant type=CSH;
    RUN;"

  ## run the SAS code, and capture the output
  sas_result <- sasr::run_sas(sas_code)
  cov_mat_df <- sasr::sd2df("cov_parms")

  ## prepare the fit object
  return(list(fit = cov_mat_df))

}
