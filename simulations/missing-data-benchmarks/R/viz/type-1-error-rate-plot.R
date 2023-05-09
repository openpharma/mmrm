# A function for plotting any given meal's empirical type 1 rate results.
type_1_error_rate_plot_fun <- function(eval_results) {

  ## extract the bias tibble, fix variable names for exposition
  t1_err_tbl <- eval_results$type_1_error_rate %>%
    mutate(
      coefficient_num = as.numeric(stringr::str_extract(coefficient, "[0-9].")),
      parameter = paste("Visit", coefficient_num),
      parameter = factor(parameter,
                         levels = paste("Visit", seq_len(10))),
      true_covar = ifelse(stringr::str_detect(.dgp_name, "_us"),
                          "Unstructured",
                          ifelse(stringr::str_detect(.dgp_name, "_csh"),
                                 "Comp. Sym. (Het.)", "Toeplitz (Hom.)"))
    )

  ## plot the type-1 error rates
  unstructured_plot <- t1_err_tbl %>%
    dplyr::filter(true_covar == "Unstructured") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = type_1_error_rate, colour = .method_name
    )) +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Treatment Effect") +
    ggplot2::ylab("Empirical Type-I Error Rate (100 Replicates, Bonf. Adj.)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::ggtitle("Unstructured Covariance Matrix") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  csh_plot <- t1_err_tbl %>%
    filter(true_covar == "Comp. Sym. (Het.)") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = type_1_error_rate, colour = .method_name
    )) +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Treatment Effect") +
    ggplot2::ylab("Empirical Type-I Error Rate (100 Replicates, Bonf. Adj.)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::ggtitle("Heterogeneous Compound Symmetry Covariance Matrix") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  toeph_plot <- t1_err_tbl %>%
    filter(true_covar == "Toeplitz (Hom.)") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = parameter, y = type_1_error_rate, colour = .method_name
    )) +
    ggplot2::geom_point(position = position_dodge(width = 0.7)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
    ggplot2::xlab("Treatment Effect") +
    ggplot2::ylab("Empirical Type-I Error Rate (100 Replicates, Bonf. Adj.)") +
    ggplot2::scale_colour_discrete(name = "Method") +
    ggplot2::ggtitle("Homogeneous Toeplitz Covariance Matrix") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1)
    ) +
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
