################################################################################
# Simulation with Low Levels of Missingness
################################################################################

# load required libraries
library(simChef)
library(mmrm)
library(glmmTMB)
library(nlme)
library(sasr)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(emmeans)
library(clusterGeneration)
library(ssh)
library(microbenchmark)
library(future)

# source the R scripts
sim_functions_files <- list.files(
  c("R/dgp", "R/method", "R/eval", "R/viz"),
  pattern = "*.R$", full.names = TRUE, ignore.case = TRUE
)
sapply(sim_functions_files, source)

# generate the possible covariance matrices
us_cov_mat <- compute_unstructured_matrix()
csh_cov_mat <- compute_csh_matrix()
toep_cov_mat <- toeplitz(c(1, 0.5, 0.25, 0.125, rep(0, 6)))

# set the sample size
n_obs <- 600

# dgps with no treatment effect
no_effect_us_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = us_cov_mat,
  missingness = "low"
)
no_effect_csh_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = csh_cov_mat,
  missingness = "low"
)
no_effect_toeph_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = toep_cov_mat,
  missingness = "low"
)

# dgps with small treatment effect
small_effect_us_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = us_cov_mat,
  trt_visit_coef = 0.25,
  missingness = "low"
)
small_effect_csh_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = csh_cov_mat,
  trt_visit_coef = 0.25,
  missingness = "low"
)
small_effect_toeph_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = toep_cov_mat,
  trt_visit_coef = 0.25,
  missingness = "low"
)

# dgps with moderate treatment effect
mod_effect_us_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = us_cov_mat,
  trt_visit_coef = 0.5,
  missingness = "low"
)
mod_effect_csh_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = csh_cov_mat,
  trt_visit_coef = 0.5,
  missingness = "low"
)
mod_effect_toeph_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  n_obs = n_obs,
  outcome_covar_mat = toep_cov_mat,
  trt_visit_coef = 0.5,
  missingness = "low"
)

# specify the methods
mmrm_us_meth <- create_method(.method_fun = mmrm_wrapper_fun, covar_type = "us")
mmrm_csh_meth <- create_method(
  .method_fun = mmrm_wrapper_fun, covar_type = "csh"
)
mmrm_toeph_meth <- create_method(
  .method_fun = mmrm_wrapper_fun, covar_type = "toeph"
)
glmmtmb_us_meth <- create_method(
  .method_fun = glmmtmb_wrapper_fun, covar_type = "us"
)
glmmtmb_csh_meth <- create_method(
  .method_fun = glmmtmb_wrapper_fun, covar_type = "csh"
)
glmmtmb_toeph_meth <- create_method(
  .method_fun = glmmtmb_wrapper_fun, covar_type = "toeph"
)
nlme_us_meth <- create_method(.method_fun = nlme_wrapper_fun, covar_type = "us")
nlme_csh_meth <- create_method(
  .method_fun = nlme_wrapper_fun, covar_type = "csh"
)
# proc_mixed_us_meth <- create_method(
#   .method_fun = proc_mixed_wrapper_fun, covar_type = "us"
# )
# proc_mixed_csh_meth <- create_method(
#   .method_fun = proc_mixed_wrapper_fun, covar_type = "csh"
# )
# proc_mixed_toeph_meth <- create_method(
#   .method_fun = proc_mixed_wrapper_fun, covar_type = "toeph"
# )

# specify the evaluation metrics
mean_time_eval <- create_evaluator(.eval_fun = mean_time_fun)
true_params <- list(
  "no_effect_us" = rep(0, 10),
  "no_effect_csh" = rep(0, 10),
  "no_effect_toeph" = rep(0, 10),
  "small_effect_us" = seq(from = 0.25, by = 0.25, length.out = 10),
  "small_effect_csh" = seq(from = 0.25, by = 0.25, length.out = 10),
  "small_effect_toeph" = seq(from = 0.25, by = 0.25, length.out = 10),
  "mod_effect_us" = seq(from = 0.5, by = 0.5, length.out = 10),
  "mod_effect_csh" = seq(from = 0.5, by = 0.5, length.out = 10),
  "mod_effect_toeph" = seq(from = 0.5, by = 0.5, length.out = 10)
)
bias_eval <- create_evaluator(.eval_fun = bias_fun, true_params = true_params)
variance_eval <- create_evaluator(.eval_fun = variance_fun)
convergence_rate_eval <- create_evaluator(.eval_fun = convergence_rate_fun)
coverage_eval <- create_evaluator(
  .eval_fun = coverage_fun, true_params = true_params
)
type_1_error_rate_eval <- create_evaluator(
  .eval_fun = type_1_error_rate_fun, true_params = true_params
)
type_2_error_rate_eval <- create_evaluator(
  .eval_fun = type_2_error_rate_fun, true_params = true_params
)

# Define parallelization
n_workers <- parallelly::availableCores() - 1L
future_globals <- ls()
options(future.globals.onReference = NULL)
# options(future.globals.onReference = "error")
plan(future::multicore, workers = n_workers)

# create the experiment
experiment <- create_experiment(
  name = "mmrm-benchmark-low-missingness-n-600-R",
  save_dir = "results/low-miss/n-600-R",
  future.globals = future_globals
) %>%
  add_dgp(no_effect_us_dgp, name = "no_effect_us") %>%
  add_dgp(no_effect_csh_dgp, name = "no_effect_csh") %>%
  add_dgp(no_effect_toeph_dgp, name = "no_effect_toeph") %>%
  add_dgp(small_effect_us_dgp, name = "small_effect_us") %>%
  add_dgp(small_effect_csh_dgp, name = "small_effect_csh") %>%
  add_dgp(small_effect_toeph_dgp, name = "small_effect_toeph") %>%
  add_dgp(mod_effect_us_dgp, name = "mod_effect_us") %>%
  add_dgp(mod_effect_csh_dgp, name = "mod_effect_csh") %>%
  add_dgp(mod_effect_toeph_dgp, name = "mod_effect_toeph") %>%
  add_method(mmrm_us_meth, name = "mmrm_us") %>%
  add_method(mmrm_csh_meth, name = "mmrm_csh") %>%
  add_method(mmrm_toeph_meth, name = "mmrm_toeph") %>%
  add_method(glmmtmb_us_meth, name = "glmmtmb_us") %>%
  add_method(glmmtmb_csh_meth, name = "glmmtmb_csh") %>%
  add_method(glmmtmb_toeph_meth, name = "glmmtmb_toeph") %>%
  add_method(nlme_us_meth, name = "nlme_us") %>%
  add_method(nlme_csh_meth, name = "nlme_csh") %>%
  # We don't run the SAS routines here.
  # add_method(proc_mixed_us_meth, name = "proc_mixed_us") %>%
  # add_method(proc_mixed_csh_meth, name = "proc_mixed_csh") %>%
  # add_method(proc_mixed_toeph_meth, name = "proc_mixed_toeph") %>%
  add_evaluator(mean_time_eval, name = "mean_fit_time") %>%
  add_evaluator(bias_eval, name = "bias") %>%
  add_evaluator(variance_eval, name = "variance") %>%
  add_evaluator(convergence_rate_eval, name = "convergence_rate") %>%
  add_evaluator(coverage_eval, name = "coverage_rate") %>%
  add_evaluator(type_1_error_rate_eval, name = "type_1_error_rate") %>%
  add_evaluator(type_2_error_rate_eval, name = "type_2_error_rate")

# No need for SAS here

# # Input here the name of the SAS container.
# hostname <- "sabanesd-3qamsv-eu.ocean"
# ssh::ssh_connect(hostname)
#
# # We can also be specific if there are multiple SAS containers we want to connect to.
# cfg_name <- "sascfg_personal_3.py"
# sasr.roche::setup_sasr_ocean(hostname, sascfg = cfg_name)
# options(sascfg = cfg_name)

# run the experiment
set.seed(56129)
results <- experiment$run(
  n_reps = 1000,
  save = TRUE,
  use_cached = TRUE,
  verbose = 2,
  checkpoint_n_reps = 10
)

# source("R/format-replicate-results/helpers.R")
#
# format_fit_and_save(experiment)
