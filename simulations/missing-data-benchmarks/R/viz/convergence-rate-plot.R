convergence_rate_plot_fun <- function(eval_results) {

  eval_results$convergence_rate %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .method_name, y = convergence_rate
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(.dgp_name)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Method") +
    ggplot2::ylab("Mean Fit Time in Seconds (100 Replicates)") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

}
