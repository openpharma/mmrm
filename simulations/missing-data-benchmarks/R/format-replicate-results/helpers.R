# format the fit results
format_fit_results <- function(fit_results, missingness, sample_size) {
  fit_results %>%
    transmute(
      missingness = missingness,
      sample_size = sample_size,
      effect_size = str_extract(.dgp_name, "^([^_])+"),
      rep = .rep,
      dgp_name = .dgp_name,
      method_name = .method_name,
      converged = pmap(
        .l = list(fit, .method_name, converged),
        .f = function(f, method_name, conv_status) {
          get_convergence(method_name, f, conv_status)
        }
      ),
      converged = unlist(converged),
      fit_time = fit_time,
      emmeans_output = pmap(
        .l = list(fit, .dgp_name, .method_name, data, converged),
        .f = function(f, dgp_name, method_name, dt, conv_status) {
          get_emmeans_output(method_name, f, dt, conv_status)
        }
      ),
      covmat_estimates = pmap(
        .l = list(fit, .method_name, converged),
        .f = function(f, method_name, conv_status) {
          get_cov_mat_estimate(method_name, f, conv_status)
        }
      )
    ) %>%
    unnest(cols = emmeans_output)
}
