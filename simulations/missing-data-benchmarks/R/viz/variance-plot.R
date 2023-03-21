variance_plot_fun <- function(eval_results) {

  ## extract the bias tibble, fix variable names for exposition
  variance_tbl <- eval_results$variance %>%
    mutate(
      coefficient_num = as.numeric(stringr::str_extract(coefficient, "[0-9].")),
      parameter = paste("Treatment x Visit", coefficient_num),
      parameter = factor(parameter,
                         levels = paste("Treatment x Visit", seq_len(10)))
    )

  ## plot the biases
  variance_tbl %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = variance, colour = .method_name
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(.dgp_name)) +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Interaction Coefficient") +
    ggplot2::ylab("Empirical Variance (100 Replicates)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

}
