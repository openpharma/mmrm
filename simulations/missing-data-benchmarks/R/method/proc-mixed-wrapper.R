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
#' @return A list containing the results of the call to PROC MIXED. This list is
#'   made up of: "fit", the estimated ATEs at each visit; "fit_time", the fit
#'   time in seconds; "output", a string containing the output of the call to
#'   PROC MIXED; "lsmeans_df", a data.frame of contrasts for all treatment
#'   estimates; "converged", an indicator for model convergence; "mod_info_df",
#'   a data.frame of model fit information; "log", the SAS log generated by
#'   running PROC MIXED.
proc_mixed_wrapper_fun <- function(
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
    bcva_change
  )
  rownames(df) <- NULL

  ## create SAS dataset
  sasr::df2sd(df, "sas_df")

  ## specify the SAS code: only return covariance matrix estimates for
  ## repeated measures
  if (covar_type == "csh") {
    sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out R = covmat_out
      ModelInfo = model_info ConvergenceStatus = conv_status;
    PROC MIXED DATA = sas_df METHOD = reml;
      CLASS trt(ref = '0') visit_num strata participant;
      MODEL bcva_change = base_bcva strata trt|visit_num / DDFM = SAT;
      REPEATED visit_num / subject=participant type=CSH R RCorr;
      LSMEANS visit_num*trt / pdiff slice=visit_num cl alpha = 0.05 OBSMARGINS;
    RUN;"
  } else if (covar_type == "toeph") {
    sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out R = covmat_out
      ModelInfo = model_info ConvergenceStatus = conv_status;
    PROC MIXED DATA = sas_df METHOD = reml;
      CLASS trt(ref = '0') visit_num strata participant;
      MODEL bcva_change = base_bcva strata trt|visit_num / DDFM = SAT;
      REPEATED visit_num / subject=participant type=TOEPH R RCorr;
      LSMEANS visit_num*trt / pdiff slice=visit_num cl alpha = 0.05 OBSMARGINS;
    RUN;"
  } else if (covar_type == "us") {
    sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out R = covmat_out
      ModelInfo = model_info ConvergenceStatus = conv_status;
    PROC MIXED DATA = sas_df METHOD = reml;
      CLASS trt(ref = '0') visit_num strata participant;
      MODEL bcva_change = base_bcva strata trt|visit_num / DDFM = SAT;
      REPEATED visit_num / subject=participant type=UNR R RCorr;
      LSMEANS visit_num*trt / pdiff slice=visit_num cl alpha = 0.05 OBSMARGINS;
    RUN;"
  } else {
    stop("This covariance matrix is not supported by this wrapper function.")
  }

  ## run the SAS code, and capture the output
  safe_run_sas <- purrr::safely(sasr::run_sas)
  sas_result <- safe_run_sas(sas_code)

  if (all(is.null(sas_result$error))) {
    ## extract the convergence status
    conv_status_df <- sasr::sd2df("conv_status")
    if (!is.data.frame(conv_status_df)) {
      converged <- FALSE
    } else {
      converged <- conv_status_df$Reason == "Convergence criteria met."
    }

    if (converged) {
      ## extract the lsmeans
      lsmeans_df <- sasr::sd2df("lsmeans_out")

      ## extract the model information
      mod_inf_df <- sasr::sd2df("model_info")

      ## extract the estimated covariance matrix
      covmat_df <- sasr::sd2df("covmat_out")
      ## remove the first two columns which are just "Index" and "Row",
      ## the remaining part is the matrix we need
      covmat <- as.matrix(covmat_df[, -c(1, 2)])

      ## extract the ATEs across visits
      ates_df <- sasr::sd2df("diffs_out") %>%
        dplyr::filter(visit_num == `_visit_num`) %>%
        dplyr::mutate(
          contrast = paste0(visit_num, ": ", trt, " - ", `_trt`)
        ) %>%
        dplyr::select(contrast, Estimate, StdErr, DF, tValue, Lower, Upper)
    } else {
      mod_inf_df <- data.frame(NA)
      lsmeans_df <- data.frame(NA)
      ates_df <- data.frame(NA)
      covmat <- matrix(NA)
    }
  } else {
    mod_inf_df <- data.frame(NA)
    lsmeans_df <- data.frame(NA)
    ates_df <- data.frame(NA)
    covmat <- matrix(NA)
    converged <- FALSE
  }

  ## extract the fit time
  fit_time <- sas_result$result$LOG %>%
    stringr::str_extract("(?<=user cpu time)\\s*[0-9.]+") %>%
    as.numeric()

  list(
    fit = structure(ates_df, covmat = covmat),
    # Note: this is not really used below, but otherwise the downstream
    # evaluation might fail if there is no `data` column.
    data = df,
    fit_time = fit_time,
    output = sas_result$result$LST,
    lsmeans_df = lsmeans_df,
    converged = converged,
    mod_inf_df = mod_inf_df,
    log = sas_result$result$LOG
  )
}
