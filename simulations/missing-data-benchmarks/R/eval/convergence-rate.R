convergence_rate_fun <- function(fit_results) {

  group_vars <- c(".dgp_name", ".method_name", "n_obs")
  fit_results %>%
    dplyr::mutate(
      convergence = purrr::pmap_lgl(
        .l = list(fit, conv_status_df),
        .f = function(f, conv_status) {
          get_convergence(f, conv_status)
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), convergence) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::summarise(convergence_rate = mean(convergence), .groups = "drop")
}
