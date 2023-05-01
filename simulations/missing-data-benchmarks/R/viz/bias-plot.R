bias_plot_fun <- function(eval_results) {

  ## extract the bias tibble, fix variable names for exposition
  bias_tbl <- eval_results$bias %>%
    mutate(
      coefficient_num = as.numeric(stringr::str_extract(coefficient, "[0-9].")),
      parameter = paste("Visit", coefficient_num),
      parameter = factor(parameter,
                         levels = paste("Visit", seq_len(10))),
      true_covar = ifelse(stringr::str_detect(.dgp_name, "_us"),
                          "Unstructured",
                          ifelse(stringr::str_detect(.dgp_name, "_csh"),
                                 "Comp. Sym. (Het.)", "Toeplitz (Hom.)")),
      effect_size = ifelse(stringr::str_detect(.dgp_name, "no_effect"),
                           "No Effect",
                           ifelse(stringr::str_detect(.dgp_name, "small_effect"),
                                  "Small Effect", "Moderate Effect")),
      effect_size = factor(
        effect_size, levels = c("No Effect", "Small Effect", "Moderate Effect")
      )
    )

  ## plot the biases
  unstructured_plot <- bias_tbl %>%
    dplyr::filter(true_covar == "Unstructured") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = bias, colour = .method_name
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(effect_size), scales = "free_y") +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Treatment Effect") +
    ggplot2::ylab("Empirical Bias (100 Replicates)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::ggtitle("Unstructured Covariance Matrix") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  csh_plot <- bias_tbl %>%
    filter(true_covar == "Comp. Sym. (Het.)") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = bias, colour = .method_name
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(effect_size), scales = "free_y") +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Treatment Effect") +
    ggplot2::ylab("Empirical Bias (100 Replicates)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::ggtitle("Heterogeneous Compound Symmetry Covariance Matrix") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  toeph_plot <- bias_tbl %>%
    filter(true_covar == "Toeplitz (Hom.)") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = bias, colour = .method_name
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(effect_size), scales = "free_y") +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Treatment Effect") +
    ggplot2::ylab("Empirical Bias (100 Replicates)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::ggtitle("Homogeneous Toeplitz Covariance Matrix") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  ggarrange(
    plotlist = list(unstructured_plot, csh_plot, toeph_plot),
    nrow = 3,
    common.legend = TRUE,
    legend = "right"
  )

}
