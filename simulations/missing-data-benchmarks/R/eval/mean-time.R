mean_time_fun <- function(fit_results) {
  group_vars <- c(".dgp_name", ".method_name", "n_obs")
  eval_out <- fit_results %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::summarize(mean_time = mean(time), .groups = "drop")

  return(eval_out)
}
