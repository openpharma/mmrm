get_mmrm_trt_visit_num_ests <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$estimate
}

get_glmmtmb_trt_visit_num_ests <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$estimate
}

get_nlme_trt_visit_num_ests <- function(fit, data) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional",
    data = data,
    mode = "df.error"
  )
  as.data.frame(marginal_means$contrasts)$estimate
}

get_proc_mixed_trt_visit_num_ests <- function(fit) {
  fit$Estimate
}

get_trt_visit_num_ests <- function(fit, dt) {
  fit_class <- class(fit)
  if (fit_class[1] == "mmrm")
    get_mmrm_trt_visit_num_ests(fit)
  else if (fit_class[1] == "glmmTMB")
    get_glmmtmb_trt_visit_num_ests(fit)
  else if (fit_class[1] == "gls")
    get_nlme_trt_visit_num_ests(fit, dt)
  else if (fit_class[1] == "data.frame")
    get_proc_mixed_trt_visit_num_ests(fit)
}

get_mmrm_convergence <- function(fit) {
  fit$opt_details$convergence == 0
}

get_glmmtmb_convergence <- function(fit) {
  fit$fit$convergence == 0
}

get_proc_mixed_convergence <- function(conv_status_df) {
  conv_status_df$Reason == "Convergence criteria met."
}


get_convergence <- function(fit, conv_status_df) {
  fit_class <- class(fit)
  if (fit_class[1] == "mmrm")
    get_mmrm_convergence(fit)
  else if (fit_class[1] == "glmmTMB")
    get_glmmtmb_convergence(fit)
  else if (fit_class[1] == "gls")
    NA # NOTE: There is no built-in convergence status indicator
  else if (fit_class[1] == "data.frame")
    get_proc_mixed_convergence(conv_status_df)
}
