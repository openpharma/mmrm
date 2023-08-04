# compute the 95% CI from emmeans output
get_95_ci <- function(emmeans_df) {
  emmeans_df %>%
    mutate(
      lower = estimate - 1.96 * SE,
      upper = estimate + 1.96 * SE,
    )
}

format_emmeans_df <- function(emmeans_df) {
  emmeans_df %>%
    transmute(
      visit_num = visit_num,
      estimate = estimate,
      stderr = SE,
      df = df,
      tvalue = t.ratio,
      pvalue = p.value,
      lower = lower,
      upper = upper
    )
}

# extract model fits output at each visit from mmrm fit
get_mmrm_emmeans_output <- function(fit) {

  # extract emmeans output
  marginal_means <- emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  emmeans_df <- as.data.frame(marginal_means$contrasts)

  # compute lower and upper 95% CI
  emmeans_df <- get_95_ci(emmeans_df)

  # format to resemble SAS output
  format_emmeans_df(emmeans_df)

}

# extract model fit outputs at each visit from glmmTMB fit
get_glmm_emmeans_output <- function(fit) {
  marginal_means <- emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  emmeans_df <- as.data.frame(marginal_means$contrasts)

  # compute lower and upper 95% CI
  emmeans_df <- get_95_ci(emmeans_df)

  # format to resemble SAS output
  format_emmeans_df(emmeans_df)

}

# extract model fit outputs estimates at each visit from gls fit
get_nlme_emmeans_output <- function(fit, data) {
  marginal_means <- emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional",
    data = data,
    mode = "df.error"
  )
  emmeans_df <- as.data.frame(marginal_means$contrasts)

  # compute lower and upper 95% CI
  emmeans_df <- get_95_ci(emmeans_df)

  # format to resemble SAS output
  format_emmeans_df(emmeans_df)

}

# extract model fit outputs estimates at each visit from PROC MIXED fit
get_mixed_emmeans_like_output <- function(fit) {
  fit %>% transmute(
    visit_num = str_extract(contrast, "^([^:])+"),
    estimate = Estimate,
    stderr = StdErr,
    df = DF,
    tvalue = tValue,
    pvalue = map2_dbl(tValue, DF, function(tvalue, df) {
      2 * min(c(pt(tvalue, df), pt(tvalue, df, lower.tail = FALSE)))
    }),
    lower = Lower,
    upper = Upper
  )
}

# general function for emmeans like information
get_emmeans_output <- function(method, fit, dt, converged) {
  if (get_convergence(method, fit, converged)) {
    if (str_detect(method, "mmrm")) {
      get_mmrm_emmeans_output(fit)
    } else if (str_detect(method, "glmmtmb")) {
      get_glmm_emmeans_output(fit)
    } else if (str_detect(method, "nlme")) {
      get_nlme_emmeans_output(fit, dt)
    } else if (str_detect(method, "proc_mixed")) {
      get_mixed_emmeans_like_output(fit)
    }
  } else {
    NA
  }
}

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
      )
    ) %>%
    unnest(cols = emmeans_output)
}
