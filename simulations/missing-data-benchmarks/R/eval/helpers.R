# get_trt_visit_num_ests ----

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

# get_convergence ----

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

# get_trt_visit_num_ses ----

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

# get_trt_visit_num_pvals ----

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

# get_emmeans_output ----

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

# get_cov_mat_estimate ----

# extract covariance matrix estimate from mmrm fit
get_mmrm_cov_mat_estimate <- function(fit) {
  mmrm::VarCorr(fit)
}

# extract covariance matrix estimate from glmmTMB fit
get_glmm_cov_mat_estimate <- function(fit) {
  mat_with_attrs <- glmmTMB::VarCorr(fit)[[c("cond", "participant")]]
  dim <- nrow(mat_with_attrs)
  ind <- seq_len(dim)
  mat_with_attrs[ind, ind]
}

# extract covariance matrix estimate from nlme fit
get_nlme_cov_mat_estimate <- function(fit) {
  mat_with_attrs <- nlme::getVarCov(fit)
  dim <- nrow(mat_with_attrs)
  ind <- seq_len(dim)
  mat_with_attrs[ind, ind]
}

# extract covariance matrix estimate from proc mixed fit
get_mixed_cov_mat_estimate <- function(fit) {
  # todo
}

# general function for covariance matrix estimate
get_cov_mat_estimate <- function(method, fit, converged) {
  if (get_convergence(method, fit, converged)) {
    if (str_detect(method, "mmrm")) {
      get_mmrm_cov_mat_estimate(fit)
    } else if (str_detect(method, "glmmtmb")) {
      get_glmm_cov_mat_estimate(fit)
    } else if (str_detect(method, "nlme")) {
      get_nlme_cov_mat_estimate(fit)
    } else if (str_detect(method, "proc_mixed")) {
      get_mixed_cov_mat_estimate(fit)
    }
  } else {
    NA
  }
}

