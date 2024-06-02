# format the fit results
format_fit_results <- function(fit_results) {
  fit_results %>%
    transmute(
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
    )
}

# The whole process
format_fit_and_save <- function(experiment) {
  gc()
  fit_results <- experiment$get_cached_results("fit")
  formatted_fit_results <- format_fit_results(fit_results)
  formatted_fit_results_file <- file.path(experiment$get_save_dir(), "formatted_fit_results.rds")
  saveRDS(formatted_fit_results, file = formatted_fit_results_file)
  formatted_fit_results
}




