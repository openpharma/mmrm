# source the R plotting scripts
sim_functions_files <- list.files(
  c("R/viz"), pattern = "*.R$", full.names = TRUE, ignore.case = TRUE
)
sapply(sim_functions_files, source)

# custom arguments for ggsave to avoid repitition
custom_ggsave <- function(ggplot, file_name, path) {
  ggsave(
    plot = ggplot,
    filename = file_name,
    path = path,
    scale = 1.2,
    width = 14,
    height = 12,
    dpi = "retina"
  )
}

# automatically generates and saves the plots associated with a results folder
save_plots <- function(path_to_results) {

  # load the eval results
  eval_results <- readRDS(paste0(path_to_results, "/eval_results.rds"))

  # create a folder for plots
  path_to_plots <- paste0(path_to_results, "/result-plots")
  dir.create(path_to_plots)

  # plot and save the empirical bias results
  bias_p <- eval_results %>% bias_plot_fun
  custom_ggsave(bias_p, "empirical_bias.jpeg", path_to_plots)

  # plot and save the empirical variance results
  variance_p <- eval_results %>% variance_plot_fun
  custom_ggsave(variance_p, "empirical_variance.jpeg", path_to_plots)

  # plot and save the empirical 95% coverage results
  coverage_rate_p <- eval_results %>% coverage_rate_plot_fun
  custom_ggsave(coverage_rate_p, "empirical_coverage_rate.jpeg", path_to_plots)

  # plot and save the empirical type 1 error rate results
  type_1_error_rate_p <- eval_results %>% type_1_error_rate_plot_fun
  custom_ggsave(
    type_1_error_rate_p, "empirical_type_1_error_rate.jpeg", path_to_plots
  )

  # plot and save the empirical type 2 error rate results
  type_2_error_rate_p <- eval_results %>% type_2_error_rate_plot_fun
  custom_ggsave(
    type_2_error_rate_p, "empirical_type_2_error_rate.jpeg", path_to_plots
  )

  # plot and save the mean fit time results
  mean_fit_time_p <- eval_results %>% mean_fit_time_plot_fun
  custom_ggsave(mean_fit_time_p, "empirical_mean_fit_time.jpeg", path_to_plots)

  # plot and save the convergence rate results
  convergence_rate_p <- eval_results %>% convergence_rate_plot_fun
  custom_ggsave(
    convergence_rate_p, "empirical_convergence_rate.jpeg", path_to_plots
  )
}
