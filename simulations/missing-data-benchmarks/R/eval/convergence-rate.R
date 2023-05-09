# Given the the MMRM implmentations' fits, this function computes the
# convergence rate for each estimator across all replicates.
convergence_rate_fun <- function(fit_results) {

  group_vars <- c(".dgp_name", ".method_name")
  fit_results %>%
    dplyr::mutate(
      convergence = purrr::pmap_lgl(
        .l = list(.method_name, fit,  converged),
        .f = function(method, fit, conv_status) {
          get_convergence(method, fit, conv_status)
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), convergence) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::summarise(
      convergence_rate = mean(convergence),
      .groups = "drop"
    )
}
