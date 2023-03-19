mean_time_fun <- function(fit_results) {
  group_vars <- c(".dgp_name", ".method_name")
  fit_results %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::summarize(mean_time = mean(fit_time), .groups = "drop")
}
