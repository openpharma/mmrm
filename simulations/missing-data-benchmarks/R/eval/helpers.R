# extract ATE estimates at each visit from mmrm fit
get_mmrm_trt_visit_num_ests <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$estimate
}

# extract ATE estimates at each visit from glmmTMB fit
get_glmm_trt_visit_num_ests <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$estimate
}

# extract ATE estimates at each visit from gls fit
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

# extract ATE estimates at each visit from PROC MIXED fit
get_mixed_trt_visit_num_ests <- function(fit) {
  fit$Estimate
}

# general function for extracting ATE estimates at each visit. NOTE: If any
# model fails to converge, 10 NAs are produced. If the number of repeated
# measures changes, then the number of NAs returned for failed convergences
# should reflect this.
get_trt_visit_num_ests <- function(method, fit, dt, converged) {
  if (get_convergence(method, fit, converged)) {
    if (stringr::str_detect(method, "mmrm")) {
      get_mmrm_trt_visit_num_ests(fit)
    } else if (stringr::str_detect(method, "glmmtmb")) {
      get_glmm_trt_visit_num_ests(fit)
    } else if (stringr::str_detect(method, "nlme")) {
      get_nlme_trt_visit_num_ests(fit, dt)
    } else if (stringr::str_detect(method, "proc_mixed")) {
      get_mixed_trt_visit_num_ests(fit)
    }
  } else {
    rep(NA, 10) # hard code 10, the number of visits in all simulations
  }
}

# extract convergence status from mmrm fit
get_mmrm_convergence <- function(converged) {
  converged
}

# extract convergence status from glmmTMB fit
get_glmm_convergence <- function(fit) {
  fit$fit$convergence == 0
}

# extract convergence status from gls fit
get_nlme_convergence <- function(converged) {
  converged
}

# extract convergence status from PROC MIXED fit
get_mixed_convergence <- function(converged) {
  converged
}

# general function for extracting convergence status from fits
get_convergence <- function(method, fit, converged) {
  if (stringr::str_detect(method, "mmrm")) {
    get_mmrm_convergence(converged)
  } else if (stringr::str_detect(method, "glmmtmb")) {
    get_glmm_convergence(fit)
  } else if (stringr::str_detect(method, "nlme")) {
    get_nlme_convergence(converged)
  } else if (stringr::str_detect(method, "proc_mixed")) {
    get_mixed_convergence(converged)
  }
}


# extract standard errors for ATE estimates at each visit from mmrm fit
get_mmrm_trt_visit_num_ses <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$SE
}

# extract standard errors for ATE estimates at each visit from glmmTMB fit
get_glmm_trt_visit_num_ses <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$SE
}

# extract standard errors for ATE estimates at each visit from gls fit
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

# extract standard errors for ATE estimates at each visit from PROC MIXED fit
get_mixed_trt_visit_num_ses <- function(fit) {
  fit$StdErr
}

# general function for extracting standard errors for ATE estimates at each
# visit from fits. NOTE: If any model fails to converge, 10 NAs are produced. If
# the number of repeated measures changes, then the number of NAs returned for
# failed convergences should reflect this.
get_trt_visit_num_ses <- function(method, fit, dt, converged) {
  if (get_convergence(method, fit, converged)) {
    if (stringr::str_detect(method, "mmrm")) {
      get_mmrm_trt_visit_num_ses(fit)
    } else if (stringr::str_detect(method, "glmmtmb")) {
      get_glmm_trt_visit_num_ses(fit)
    } else if (stringr::str_detect(method, "nlme")) {
      get_nlme_trt_visit_num_ses(fit, dt)
    } else if (stringr::str_detect(method, "proc_mixed")) {
      get_mixed_trt_visit_num_ses(fit)
    }
  } else {
    rep(NA, 10) # hard code 10, the number of visits in all simulations
  }
}

# extract p-values for 2-sided hypothesis tests about ATEs at each visit from
# mmrm fit
get_mmrm_trt_visit_num_pvals <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$p.value
}

# extract p-values for 2-sided hypothesis tests about ATEs at each visit from
# glmmTMB fit
get_glmm_trt_visit_num_pvals <- function(fit) {
  marginal_means <- emmeans::emmeans(
    fit,
    spec = trt.vs.ctrl ~ trt | visit_num,
    weights = "proportional"
  )
  as.data.frame(marginal_means$contrasts)$p.value
}

# extract p-values for 2-sided hypothesis tests about ATEs at each visit from
# gls fit
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

# extract p-values for 2-sided hypothesis tests about ATEs at each visit from
# PROC MIXED fit
get_mixed_trt_visit_num_pvals <- function(fit) {
  quantiles <- pt(fit$tValue, fit$DF)
  sapply(quantiles, function(q) 2 * min(q, 1 - q))
}

# general function for extracting p-values for 2-sided hypothesis tests about
# ATEs at each visit from fits. NOTE: If any model fails to converge, 10 NAs are
# produced. If the number of repeated measures changes, then the number of NAs
# returned for failed convergences should reflect this.
get_trt_visit_num_pvals <- function(method, fit, dt, converged) {
  if (get_convergence(method, fit, converged)) {
    if (stringr::str_detect(method, "mmrm")) {
      get_mmrm_trt_visit_num_pvals(fit)
    } else if (stringr::str_detect(method, "glmmtmb")) {
      get_glmm_trt_visit_num_pvals(fit)
    } else if (stringr::str_detect(method, "nlme")) {
      get_nlme_trt_visit_num_pvals(fit, dt)
    } else if (stringr::str_detect(method, "proc_mixed")) {
      get_mixed_trt_visit_num_pvals(fit)
    }
  } else {
    rep(NA, 10) # hard code 10, the number of visits in all simulations
  }
}
