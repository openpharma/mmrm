# Given the the MMRM implmentations' fits, this function computes the variance
# of the ATE estimators at each visit.
variance_fun <- function(fit_results) {
  group_vars <- c(".dgp_name", ".method_name")
  fit_results %>%
    dplyr::mutate(
      estimates = purrr::pmap(
        .l = list(fit, .dgp_name, .method_name, data, converged),
        .f = function(f, dgp_name, method_name, dt, conv_status) {
          # extract the trt:vist_num estimates
          estimates <- get_trt_visit_num_ests(method_name, f, dt, conv_status)

          names(estimates) <- paste0(
            "trt_visit_num",
            stringr::str_pad(seq_len(length(estimates)), width = 2, pad = "0")
          )
          estimates
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), estimates) %>%
    tidyr::unnest_wider(col = estimates) %>%
    dplyr::group_by(dplyr::across({{ group_vars }})) %>%
    dplyr::summarise(
      dplyr::across(dplyr::contains("trt_visit_num"), var, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("trt_visit_num"),
      names_to = "coefficient",
      values_to = "variance"
    )
}
