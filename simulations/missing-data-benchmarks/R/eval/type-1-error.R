type_1_error_rate_fun <- function(fit_results, true_params) {

  # identify DGPs with null treatment * visit number effects
  dgp_names <- names(true_params)
  null_dgp_idx <- sapply(dgp_names, function(dgp) all(true_params[[dgp]] == 0))
  null_dgps <- dgp_names[null_dgp_idx]

  # compute type 1 error rate
  group_vars <- c(".dgp_name", ".method_name", "n_obs")
  fit_results %>%
    dplyr::filter(.dgp_name %in% null_dgps) %>%
    dplyr::mutate(
      error = purrr::pmap(
        .l = list(fit, .dgp_name, data),
        .f = function(f, dgp_name, dt) {

          # extract the trt:vist_num p-values
          pvals <- get_trt_visit_num_pvals(f, dt)

          # compute the errors
          err <- p.adjust(pvals, method = "bonferroni") < 0.05
          names(err) <- paste0(
            "trt_visit_num",
            stringr::str_pad(seq_len(length(pvals)), width = 2, pad = "0")
          )
          err
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), error) %>%
    tidyr::unnest_wider(col = error) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::summarise(
      dplyr::across(dplyr::contains("trt_visit_num"), mean),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("trt_visit_num"),
      names_to = "coefficient",
      values_to = "type_1_error_rate"
    )
}
