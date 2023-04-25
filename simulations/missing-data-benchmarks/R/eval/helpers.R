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

get_mmrm_convergence <- function(converged) {
  converged
}

get_glmmtmb_convergence <- function(fit) {
  fit$fit$convergence == 0
}

get_nlme_convergence <- function(converged) {
  converged
}

get_proc_mixed_convergence <- function(converged) {
  converged
}


get_convergence <- function(fit, converged) {
  fit_class <- class(fit)
  if (fit_class[1] == "mmrm")
    get_mmrm_convergence(fit)
  else if (fit_class[1] == "glmmTMB")
    get_glmmtmb_convergence(fit)
  else if (fit_class[1] == "gls" || fit_class[1] == "NULL")
    get_nlme_convergence(converged)
  else if (fit_class[1] == "data.frame")
    get_proc_mixed_convergence(converged)
}

get_mmrm_trt_visit_num_ses <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$SE
}

get_glmmtmb_trt_visit_num_ses <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$SE
}

get_nlme_trt_visit_num_ses <- function(fit, data) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional",
    data = data,
    mode = "df.error"
  )
  as.data.frame(marginal_means$contrasts)$SE
}

get_proc_mixed_trt_visit_num_ses <- function(fit) {
  fit$StdErr
}

get_trt_visit_num_ses <- function(fit, dt) {
  fit_class <- class(fit)
  if (fit_class[1] == "mmrm")
    get_mmrm_trt_visit_num_ses(fit)
  else if (fit_class[1] == "glmmTMB")
    get_glmmtmb_trt_visit_num_ses(fit)
  else if (fit_class[1] == "gls")
    get_nlme_trt_visit_num_ses(fit, dt)
  else if (fit_class[1] == "data.frame")
    get_proc_mixed_trt_visit_num_ses(fit)
}

get_mmrm_trt_visit_num_pvals <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$p.value
}

get_glmmtmb_trt_visit_num_pvals <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$p.value
}

get_nlme_trt_visit_num_pvals <- function(fit, data) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional",
    data = data,
    mode = "df.error"
  )
  as.data.frame(marginal_means$contrasts)$p.value
}

get_proc_mixed_trt_visit_num_pvals <- function(fit) {
  quantiles <- pt(fit$tValue, fit$DF)
  sapply(quantiles, function(q) 2 * min(q, 1 - q))
}

get_trt_visit_num_pvals <- function(fit, dt) {
  fit_class <- class(fit)
  if (fit_class[1] == "mmrm")
    get_mmrm_trt_visit_num_pvals(fit)
  else if (fit_class[1] == "glmmTMB")
    get_glmmtmb_trt_visit_num_pvals(fit)
  else if (fit_class[1] == "gls")
    get_nlme_trt_visit_num_pvals(fit, dt)
  else if (fit_class[1] == "data.frame")
    get_proc_mixed_trt_visit_num_pvals(fit)
}
