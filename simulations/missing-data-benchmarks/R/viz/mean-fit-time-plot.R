# A function for plotting any given meal's mean fit time results.
mean_fit_time_plot_fun <- function(eval_results) {
  fit_time_tbl <- eval_results$mean_fit_time %>%
    mutate(
      true_covar = ifelse(stringr::str_detect(.dgp_name, "_us"),
        "Unstructured",
        ifelse(stringr::str_detect(.dgp_name, "_csh"),
          "Comp. Sym. (Het.)", "Toeplitz (Hom.)"
        )
      ),
      effect_size = ifelse(stringr::str_detect(.dgp_name, "no_effect"),
        "No Effect",
        ifelse(stringr::str_detect(.dgp_name, "small_effect"),
          "Small Effect", "Moderate Effect"
        )
      ),
      effect_size = factor(
        effect_size,
        levels = c("No Effect", "Small Effect", "Moderate Effect")
      )
    )

  unstructured_plot <- fit_time_tbl %>%
    dplyr::filter(true_covar == "Unstructured") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .method_name, y = mean_time
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(effect_size)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Method") +
    ggplot2::ylab("Mean Fit Time in Seconds (100 Replicates)") +
    ggplot2::ggtitle("Unstructured Covariance Matrix") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  csh_plot <- fit_time_tbl %>%
    dplyr::filter(true_covar == "Comp. Sym. (Het.)") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .method_name, y = mean_time
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(effect_size)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Method") +
    ggplot2::ylab("Mean Fit Time in Seconds (100 Replicates)") +
    ggplot2::ggtitle("Heterogeneous Compound Symmetry Covariance Matrix") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  toeph_plot <- fit_time_tbl %>%
    dplyr::filter(true_covar == "Toeplitz (Hom.)") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .method_name, y = mean_time
    )) +
    ggplot2::facet_grid(rows = ggplot2::vars(effect_size)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Method") +
    ggplot2::ylab("Mean Fit Time in Seconds (100 Replicates)") +
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
