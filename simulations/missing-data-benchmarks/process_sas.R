args <- R.utils::commandArgs(asValues = TRUE, excludeReserved = TRUE)

input_data <- SASxport::read.xport(args$input_file, names.tolower = TRUE)
output_data <- args$output_file

library(dplyr)

method_covar <- args$cov
covar_rsp <- args$covar_rsp
reml <- args$reml

fit_time <- input_data$fittime %>%
  mutate(fit_time = dur, src = substr(src, 4, 8)) %>%
  mutate(fit_time = if_else(fit_time < 0, fit_time + 24 * 60 * 60, fit_time))
convergence <- input_data$conv %>%
  mutate(converged = status == 0, src = substr(src, 4, 8)) %>%
  select(src, converged)
emmeans <- input_data$diffs %>%
  filter(trt == 1, x.trt == 0, visit == x.visit) %>%
  mutate(src = substr(src, 4, 8)) %>%
  group_by(src) %>%
  summarize(
    emmeans_output = list(data.frame(visit, estimate, stderr, df, tvalue, pvalue = probt, lower, upper))
  )

cov_from_parm <- function(x, type) {
  type <- type[1]
  if (type == "csh") {
    nl <- length(x) - 1
    d <- matrix(tail(x, 1), nl, nl)
    diag(d) <- 1
    v <- sqrt(head(x, nl))
    outer(v, v) * d
  } else if (type == "toeph") {
    nl <- (length(x) + 1) / 2
    d <- toeplitz(c(1, tail(x, nl - 1)))
    v <- sqrt(head(x, nl))
    outer(v, v) * d
  } else if (type == "us") {
    nl <- floor(sqrt(2 * length(x)))
    v <- sqrt(head(x, nl))
    d <- matrix(0, nl, nl)
    d[upper.tri(d, diag = FALSE)] <- tail(x, nl * (nl - 1) / 2)
    d <- d + t(d)
    diag(d) <- 1
    outer(v, v) * d
  }
}

cov_csh <- input_data$cvparm %>%
  mutate(src = substr(src, 4, 8)) %>%
  group_by(src) %>%
  summarize(covmat_estimates = list(cov_from_parm(estimate, method_covar)))

df <- fit_time %>%
  full_join(convergence, by = "src") %>%
  full_join(emmeans, by = "src") %>%
  full_join(cov_csh, by = "src") %>%
  mutate(missingness = miss, treatment_effect = trt.eff) %>%
  mutate(method_covar = method_covar, method = "sas", rep = as.numeric(src)) %>%
  mutate(reml = reml, covar_rsp = covar_rsp) %>%
  select(fit_time, converged, rep, method_covar, method, missingness, n, treatment_effect, seed, emmeans_output, covmat_estimates, covar_rsp, reml)

saveRDS(df, output_data)
