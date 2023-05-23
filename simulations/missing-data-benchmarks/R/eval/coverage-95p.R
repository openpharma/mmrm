# Given the the MMRM implmentations' fits, this function computes the 95%
# coverage rate of the ATE estimators at each visit. The true ATEs must be
# provided via the true_params functions argument.
coverage_fun <- function(fit_results, true_params) {
  group_vars <- c(".dgp_name", ".method_name")
  fit_results %>%
    dplyr::mutate(
      covered = purrr::pmap(
        .l = list(.method_name, fit, .dgp_name, data, converged),
        .f = function(method_name, f, dgp_name, dt, conv_status) {
          # extract the trt:vist_num ses and estimates
          ses <- get_trt_visit_num_ses(method_name, f, dt, conv_status)
          estimates <- get_trt_visit_num_ests(method_name, f, dt, conv_status)

          # extract the true parameter values
          true_values <- true_params[[dgp_name]]

          # check if lower and upper ci bounds using an alpha of 0.05
          lower_cis <- estimates - 1.96 * ses
          upper_cis <- estimates + 1.96 * ses

          # check if true parameters are covered
          covered <- (lower_cis < true_values) & (upper_cis > true_values)

          # name appropriately and return
          names(covered) <- paste0(
            "trt_visit_num",
            stringr::str_pad(seq_len(length(ses)), width = 2, pad = "0")
          )
          covered
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), covered) %>%
    tidyr::unnest_wider(col = covered) %>%
    dplyr::group_by(dplyr::across({{ group_vars }})) %>%
    dplyr::summarise(
      dplyr::across(dplyr::contains("trt_visit_num"), mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("trt_visit_num"),
      names_to = "coefficient",
      values_to = "coverage"
    )
}
