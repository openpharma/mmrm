# Given the the MMRM implmentations' fits, this function computes the bias of
# the ATE estimators at each visit. The true ATEs must be provided via the
# true_params functions argument.
bias_fun <- function(fit_results, true_params) {
  group_vars <- c(".dgp_name", ".method_name")
  fit_results %>%
    dplyr::mutate(
      error = purrr::pmap(
        .l = list(fit, .dgp_name, .method_name, data, converged),
        .f = function(f, dgp_name, method_name, dt, conv_status) {
          # extract the trt:vist_num estimates
          estimates <- get_trt_visit_num_ests(method_name, f, dt, conv_status)

          # extract the true parameter values
          true_values <- true_params[[dgp_name]]

          # compute the errors
          err <- estimates - true_values
          names(err) <- paste0(
            "trt_visit_num",
            stringr::str_pad(seq_len(length(estimates)), width = 2, pad = "0")
          )
          err
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), error) %>%
    tidyr::unnest_wider(col = error) %>%
    dplyr::group_by(dplyr::across({{ group_vars }})) %>%
    dplyr::summarise(
      dplyr::across(dplyr::contains("trt_visit_num"), mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("trt_visit_num"),
      names_to = "coefficient",
      values_to = "bias"
    )
}
