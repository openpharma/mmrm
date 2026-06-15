################################################################################
## Simulation Script
################################################################################

## load required libraries
library(here)
library(simChef)
library(future)
library(MASS)
library(tibble)
library(mmrm)
library(nlme)
library(glmmTMB)
library(sasr)
library(dplyr)
library(tidyr)
library(rtables)
library(ggplot2)

## NOTE: sasr doens't seem to play well with future... run simulations
## sequentially for now
plan(sequential)

## specify the data-generating processes
source(here("simulations/csh-benchmarks/R/dgp-functions.R"))
hom_rct_dgp <- create_dgp(
  .dgp_fun = rct_dgp_fun,
  outcome_vars = rep(1, 10),
  outcome_cor = 0.5
)
het_rct_dgp <- create_dgp(,
  .dgp_fun = rct_dgp_fun,
  outcome_vars = seq(from = 0.5, by = 0.25, length.out = 10),
  outcome_cor = 0.2
)

## define the true covariance matrix of the DGPs
true_covar_ls <- list(
  "hom_rct" = compute_true_covar_mat(rep(1, 10), 0.5),
  "het_rct" = compute_true_covar_mat(
    seq(from = 0.5, by = 0.25, length.out = 10),
    0.2
  )
)

## specify the methods
source(here("simulations/csh-benchmarks/R/method-functions.R"))
mrmm_method <- create_method(.method_fun = mmrm_wrapper_fun)
glmmtmb_method <- create_method(.method_fun = glmmtmb_wrapper_fun)
nlme_method <- create_method(.method_fun = nlme_wrapper_fun)
proc_mixed_method <- create_method(.method_fun = proc_mixed_fun)

## specify the evaluation metrics
source(here("simulations/csh-benchmarks/R/eval-functions.R"))
frobenius_loss_eval <- create_evaluator(
  .eval_fun = frobenius_loss_fun,
  true_covar_mat_ls = true_covar_ls
)
spectral_loss_eval <- create_evaluator(
  .eval_fun = spectral_loss_fun,
  true_covar_mat_ls = true_covar_ls
)
sq_err_loss_eval <- create_evaluator(
  .eval_fun = csh_param_sq_err_loss_fun,
  true_covar_mat_ls = true_covar_ls
)
bias_eval <- create_evaluator(
  .eval_fun = csh_param_bias_fun,
  true_covar_mat_ls = true_covar_ls
)

## specify the result summarizers
source(here("simulations/csh-benchmarks/R/visualizer-functions.R"))
risk_tbl_viz <- create_visualizer(.viz_fun = risk_tbl_fun)
loss_dist_viz <- create_visualizer(.viz_fun = loss_dist_fun)
mse_viz <- create_visualizer(.viz_fun = mse_fun)
bias_viz <- create_visualizer(.viz_fun = bias_fun)

## define the experiment object
experiment <- create_experiment(
  name = "covar-matrix-estimation-comparison",
  save_dir = "results"
) %>%
  add_dgp(hom_rct_dgp, name = "hom_rct") %>%
  add_dgp(het_rct_dgp, name = "het_rct") %>%
  add_vary_across(.dgp = "hom_rct", num_part = c(250, 500, 1000)) %>%
  add_vary_across(.dgp = "het_rct", num_part = c(250, 500, 1000)) %>%
  add_method(mrmm_method, name = "mmrm") %>%
  add_method(glmmtmb_method, name = "glmmTMB") %>%
  add_method(nlme_method, name = "nlme") %>%
  add_method(proc_mixed_method, name = "proc_mixed") %>%
  add_evaluator(frobenius_loss_eval, name = "frobenius_loss") %>%
  add_evaluator(spectral_loss_eval, name = "spectral_loss") %>%
  add_evaluator(sq_err_loss_eval, name = "sq_err_loss") %>%
  add_evaluator(bias_eval, name = "biases") %>%
  add_visualizer(risk_tbl_viz, name = "risk_tibble") %>%
  add_visualizer(loss_dist_viz, name = "loss_dist_plot") %>%
  add_visualizer(mse_viz, name = "mse_plot") %>%
  add_visualizer(bias_viz, name = "bias_plot")

## run the experiment
set.seed(510)
results <- experiment$run(
  n_reps = 100,
  future.globals = c(
    "compute_true_covar_mat",
    "compute_sasr_covar_mat",
    "compute_glmmtmb_covar_mat",
    "get_covar_mat"
  ),
  save = TRUE
)
