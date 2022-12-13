################################################################################
## Visualizer functions
################################################################################

#' Generate risks table
#'
#' @description This function estimates the covariance matrix estimators' risks
#'   using the losses computed by the simChef evaluators. The Frobenius and
#'   spectral norm risks are returned, stratified, by data-generating process,
#'   sample size, and fit method.
#'
#' @param eval_results A list of tibbles containing the results of the evaluator
#'   functions.
#'
#' @return A rtable object reporting the estimated Frobenius and spectral risks
#'   for every combination of data-generating process, sample size, and
#'   estimator.
risk_tbl_fun <- function(eval_results) {

  ## strata variables
  join_vars <- c(".rep", ".dgp_name", ".method_name", "num_part")
  strata_vars <- c(".dgp_name", ".method_name", "num_part")

  ## combine the loss tables
  loss_tbl <- eval_results$frobenius_loss %>%
    dplyr::left_join(
      eval_results$spectral_loss,
      by = join_vars
    ) %>%
    tidyr::pivot_longer(
      cols = c(frobenius_loss, spectral_loss),
      values_to = "value", names_to = "type"
    ) %>%
    dplyr::mutate(
      .dgp_name = ifelse(
        .dgp_name == "het_rct", "Heteroscedastic RCT", "Homoscedastic RCT"
      ),
      .dgp_name = factor(.dgp_name,
        levels = c("Homoscedastic RCT", "Heteroscedastic RCT")
      ),
      type = ifelse(
        type == "frobenius_loss", "Frobenius Risk", "Spectral Risk"
      ),
      type = factor(type, levels = c("Frobenius Risk", "Spectral Risk")),
      n = paste0("n = ", num_part),
      n = factor(n, levels = paste0("n = ", unique(.data$num_part)))
    )

  ## compute the risks under each loss function and generate a table
  risk_fun <- function(df, labelstr) {
    rtables::rcell(mean(df$value), label = labelstr, format = "xx.xxxx")
  }
  tbl_recipe <- rtables::basic_table() %>%
    rtables::split_cols_by(".dgp_name") %>%
    rtables::split_cols_by("type") %>%
    rtables::split_rows_by("n") %>%
    rtables::split_rows_by(".method_name") %>%
    rtables::summarize_row_groups(cfun = risk_fun)

  return(rtables::build_table(tbl_recipe, loss_tbl))

}

#' Plot loss distributions
#'
#' @description This function plots the covariance matrix estimators' empirical
#'   loss distributions. The losses are computed by the simChef evaluators. The
#'   Frobenius and spectral norm losses are considered, stratified, by data-generating
#'   process, sample size, and fit method.
#'
#' @param eval_results A list of tibbles containing the results of the evaluator
#'   functions.
#'
#' @return A ggplot2 object depicting the stratified empirical loss distributions.
loss_dist_fun <- function(eval_results) {

  ## strata variables
  join_vars <- c(".rep", ".dgp_name", ".method_name", "num_part")

  ## combine the loss tables
  loss_tbl <- eval_results$frobenius_loss %>%
    dplyr::left_join(
      eval_results$spectral_loss,
      by = join_vars
    ) %>%
    tidyr::pivot_longer(
      cols = c(frobenius_loss, spectral_loss),
      values_to = "loss", names_to = "type"
    ) %>%
    dplyr::mutate(
      .dgp_name = ifelse(
        .dgp_name == "het_rct", "Heteroscedastic RCT", "Homoscedastic RCT"
      ),
      .dgp_name = factor(.dgp_name,
        levels = c("Homoscedastic RCT", "Heteroscedastic RCT")
      ),
      type = if_else(
        type == "frobenius_loss", "Frobenius Loss", "Spectral Loss"
      )
    )

  ## plot the distribution of losses
  loss_tbl %>%
    ggplot2::ggplot(ggplot2::aes(
      x = as.factor(num_part), y = loss, fill = .method_name
    )) +
    ggplot2::geom_violin(draw_quantiles = 0.5) +
    ggplot2::facet_grid(
      cols = vars(.dgp_name), rows = vars(type), scales = "free_y"
    ) +
    ggplot2::xlab("Sample Size") +
    ggplot2::ylab("Empirical Distribution (500 Replicates)") +
    ggplot2::scale_fill_discrete(name = "Method") +
    ggplot2::theme_bw()

}
