# load required libraries
library(simChef)
library(mmrm)
library(glmmTMB)
library(nlme)
library(sasr)
library(stringr)
library(dplyr)

# source the R scripts
sim_functions_files = list.files(
  c("R/dgp", "R/method", "R/eval", "R/viz"),
  pattern = "*.R$", full.names = TRUE, ignore.case = TRUE
)
sapply(sim_functions_files, source)

# specify the DGPs
us_cov_mat <- compute_unstructured_matrix()
no_miss_no_effect_us_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  outcome_covar_mat = us_cov_mat
)
csh_cov_mat <- compute_csh_matrix()
no_miss_no_effect_csh_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  outcome_covar_mat = csh_cov_mat
)
toep_cov_mat <- toeplitz(c(1, 0.5, 0.25, 0.125, rep(0, 6)))
no_miss_no_effect_toeph_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  outcome_covar_mat = toep_cov_mat
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
proc_mixed_us_meth <- create_method(
  .method_fun = proc_mixed_wrapper_fun, covar_type = "us"
)
proc_mixed_csh_meth <- create_method(
  .method_fun = proc_mixed_wrapper_fun, covar_type = "csh"
)
proc_mixed_toeph_meth <- create_method(
  .method_fun = proc_mixed_wrapper_fun, covar_type = "toeph"
)

# specify the evaluation metrics
mean_time_eval <- create_evaluator(.eval_fun = mean_time_fun)

# specify the resul summarizers

# create the experiment
experiment <- create_experiment(
  name = "mmrm-benchmark", save_dir = "results"
) %>%
  add_dgp(no_miss_no_effect_us_dgp, name = "no_miss_no_effect_us") %>%
  add_dgp(no_miss_no_effect_csh_dgp, name = "no_miss_no_effect_csh") %>%
  add_dgp(no_miss_no_effect_toeph_dgp, name = "no_miss_no_effect_toeph") %>%
  add_vary_across(
    .dgp = c(
      "no_miss_no_effect_us", "no_miss_no_effect_csh", "no_miss_no_effect_toeph"
    ),
    n_obs = c(125, 250)
  ) %>%
  add_method(mmrm_us_meth, name = "mmrm_us") %>%
  add_method(mmrm_csh_meth, name = "mmrm_csh") %>%
  add_method(mmrm_toeph_meth, name = "mmrm_toeph")  %>% # NOTE: sometimes causes errors
  add_method(glmmtmb_us_meth, name = "glmmtmb_us") %>%
  add_method(glmmtmb_csh_meth, name = "glmmtmb_csh") %>%
  add_method(glmmtmb_toeph_meth, name = "glmmtmb_toeph") %>%
  add_method(nlme_us_meth, name = "nlme_us") %>%
  add_method(nlme_csh_meth, name = "nlme_csh") %>%
  ## add_method(proc_mixed_us_meth, name = "proc_mixed_us") %>%
  ## add_method(proc_mixed_csh_meth, name = "proc_mixed_csh") %>%
  ## add_method(proc_mixed_toeph_meth, name = "proc_mixed_toeph") %>%
  add_evaluator(mean_time_eval, name = "mean_fit_time")

# run the experiment
set.seed(72342)
results <- experiment$run(
  n_reps = 2
)
