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

get_trt_visit_num_ests <- function(fit) {
  fit_class <- class(fit)
  dplyr::case_when(
    fit_class[1] == "mmrm" ~ get_mmrm_trt_visit_num_ests(fit),
    fit_class[1] == "glmmTMB" ~ get_glmmtmb_trt_visit_num_ests(fit),
    fit_class[1] == "gls" ~ get_nlme_trt_visit_num_ests(fit),
    fit_class[1] == "data.frame" ~ get_proc_mixed_trt_visit_num_ests(fit)
  )
}
