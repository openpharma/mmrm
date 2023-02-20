#' PROC MIXED Function (SAS)
#'
#' @description This function takes as input the ouput of a data-generating
#'   process function, and fits PROC MIXED repeated measures model on this data.
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
#' @return Estimated parameters of the repeated measures' covariance matrix.
#'   These parameters are stored in a data.frame object.
proc_mixed_wrapper_fun <- function(
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
    bcva_change
  )
  rownames(df) <- NULL

  ## create SAS dataset
  sasr::df2sd(df, "sas_df")

  ## specify the SAS code: only return covariance matrix estimates for
  ## repeated measures
  if (covar_type == "csh") {
    sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out
      ModelInfo = model_info ConvergenceStatus = conv_status;
    PROC MIXED DATA = sas_df METHOD = reml;
      CLASS trt(ref = '0') visit_num strata participant;
      MODEL bcva_change = base_bcva strata trt|visit_num / DDFM = KR;
      REPEATED visit_num / subject=participant type=CSH R RCorr;
      LSMEANS visit_num*trt / pdiff slice=visit_num cl alpha = 0.05 OBSMARGINS;
    RUN;"
  } else if (covar_type == "toeph") {
    sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out
      ModelInfo = model_info ConvergenceStatus = conv_status;
    PROC MIXED DATA = sas_df METHOD = reml;
      CLASS trt(ref = '0') visit_num strata participant;
      MODEL bcva_change = base_bcva strata trt|visit_num / DDFM = KR;
      REPEATED visit_num / subject=participant type=TOEPH R RCorr;
      LSMEANS visit_num*trt / pdiff slice=visit_num cl alpha = 0.05 OBSMARGINS;
    RUN;"
  }  else if (covar_type == "us") {
    sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out
      ModelInfo = model_info ConvergenceStatus = conv_status;
    PROC MIXED DATA = sas_df METHOD = reml;
      CLASS trt(ref = '0') visit_num strata participant;
      MODEL bcva_change = base_bcva strata trt|visit_num / DDFM = KR;
      REPEATED visit_num / subject=participant type=UNR R RCorr;
      LSMEANS visit_num*trt / pdiff slice=visit_num cl alpha = 0.05 OBSMARGINS;
    RUN;"
  } else {
    stop("This covariance matrix is not supported by this wrapper function.")
  }

  ## run the SAS code, and capture the output
  sas_result <- sasr::run_sas(sas_code)

  ## extract the model information
  mod_inf_df <- sasr::sd2df("model_info")

  ## extract the convergence status
  conv_status_df <- sasr::sd2df("conv_status")

  ## extract the lsmeans
  lsmeans_df <- sasr::sd2df("lsmeans_out")

  ## extract the ATEs across visits
  ates_df <- sasr::sd2df("diffs_out") %>%
    dplyr::filter(visit_num == `_visit_num`) %>%
    dplyr::mutate(
      contrast = paste0(visit_num,": ", trt, " - ", `_trt`)
    ) %>%
    dplyr::select(contrast, Estimate, StdErr, DF, tValue, Lower, Upper)

  ## extract the fit time
  fit_time <- sas_result$LOG %>%
    stringr::str_extract("(?<=user cpu time)\\s*[0-9.]+") %>%
    as.numeric()

  return(list(
    "fit" = ates_df,
    "fit_time" = fit_time,
    "output" = sas_result$LST,
    "lsmeans_df" = lsmeans_df,
    "conv_status_df" = conv_status_df,
    "mod_inf_df" = mod_inf_df,
    "log" = sas_result$LOG
  ))

}
