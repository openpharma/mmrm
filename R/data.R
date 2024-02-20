# fev_data ----

#' Example Data on FEV1
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @note Measurements of FEV1 (forced expired volume in one second) is a measure
#' of how quickly the lungs can be emptied. Low levels of FEV1 may indicate
#' chronic obstructive pulmonary disease (COPD).
#'
#' @format A `tibble` with 800 rows and 7 variables:
#'   - `USUBJID`: subject ID.
#'   - `AVISIT`: visit number.
#'   - `ARMCD`: treatment, `TRT` or `PBO`.
#'   - `RACE`: 3-category race.
#'   - `SEX`: sex.
#'   - `FEV1_BL`: FEV1 at baseline (%).
#'   - `FEV1`: FEV1 at study visits.
#'   - `WEIGHT`: weighting variable.
#'   - `VISITN`: integer order of the visit.
#'   - `VISITN2`: coordinates of the visit for distance calculation.
#'
#' @source This is an artificial dataset.
"fev_data"

# bcva_data ----

#' Example Data on BCVA
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @note Measurements of BCVA (best corrected visual acuity) is a measure of how
#'   how many letters a person can read off of an eye chart using corrective
#'   lenses or contacts. This a common endpoint in ophthalmology trials.
#'
#' @format A `tibble` with 10,000 rows and 7 variables:
#'   - `USUBJID`: subject ID.
#'   - `VISITN`: visit number (numeric).
#'   - `AVISIT`: visit number (factor).
#'   - `ARMCD`: treatment, `TRT` or `CTL`.
#'   - `RACE`: 3-category race.
#'   - `BCVA_BL`: BCVA at baseline.
#'   - `BCVA_CHG`: Change in BCVA at study visits.
#'
#' @source This is an artificial dataset.
"bcva_data"

#' Cache Data for `mmrm` Model Comparison
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @note The cached data for comparison is used for the vignettes generation.
#' Please make sure that this data is refreshed before each package release
#' by running the script `data-raw/mmrm_review.R`.
#' Please make sure to install the `mmrm` package instead of using
#' `devtools::load_all()` before running the script to achieve accurate timings.
#'
#' @format A `list` with following elements:
#'   - `conv_time_fev`: Convergence time on FEV data.
#'   - `conv_time_bcva`: Convergence time on BCVA data.
#'   - `rel_diff_ests_tbl_fev`: Relative difference in estimates on FEV data.
#'   - `rel_diff_ests_tbl_bcva`: Relative difference in estimates on BCVA data.
#'   - `conv_rate`: Convergence rate on data with different missing levels.
#'   - `df_missingness`: Summary of missingness on simulated data.
#'
#' @keywords internal
#' @source This is created based on simulations on FEV data and BCVA data.
"cached_mmrm_results"
