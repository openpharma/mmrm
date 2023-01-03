################################################################################
## Method Functions Tests
################################################################################

## load required libraries
library(here)
library(testthat)
library(stringr)

## load data-generating function and method wrapper functions
source(here("simulations/csh-benchmarks/R/dgp-functions.R"))
source(here("simulations/csh-benchmarks/R/method-functions.R"))

test_that("mmrm wrapper estimates a treatment effect close to ground truth", {
  library(mmrm)
  set.seed(612342)
  trt_effect <- 2
  dt_ls <- rct_dgp_fun(num_part = 1000, fixed_trt = trt_effect)
  output <- mmrm_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_trt_effect <- as.numeric(output$fit$beta_est["trt"])
  expect_equal(est_trt_effect, trt_effect, tolerance = 0.1)
})

test_that("mmrm wrapper estimates a CSH parameters close to ground truth", {
  library(mmrm)
  set.seed(2612)
  num_rep_meas <- 10
  outcome_vars <- seq_len(num_rep_meas)
  outcome_cor <- 0.2
  dt_ls <- rct_dgp_fun(
    num_part = 1000,
    num_rep_meas = num_rep_meas,
    outcome_vars = outcome_vars,
    outcome_cor = outcome_cor
  )
  output <- mmrm_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_outcome_vars <- diag(output$fit$cov)
  est_outcome_cor <- output$fit$cov[1, 2] /
    sqrt(output$fit$cov[1, 1] * output$fit$cov[2, 2])
  var_names <- str_pad(seq_len(num_rep_meas), 2, "left", "0")
  expect_equal(names(est_outcome_vars), var_names)
  expect_equal(as.numeric(est_outcome_vars), outcome_vars, tolerance = 0.1)
  expect_equal(est_outcome_cor, outcome_cor, tolerance = 0.1)
})

test_that("glmmTMB wrapper estimates a treatment effect close to ground truth", {
  library(glmmTMB)
  set.seed(790134)
  trt_effect <- 2
  dt_ls <- rct_dgp_fun(num_part = 1000, fixed_trt = trt_effect)
  output <- glmmTMB_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_trt_effect <- as.numeric(fixef(output$fit)$cond["trt"])
  expect_equal(est_trt_effect, trt_effect, tolerance = 0.1)
})

test_that("glmmTMB wrapper estimates a CSH parameters close to ground truth", {
  library(glmmTMB)
  set.seed(951831)
  num_rep_meas <- 10
  outcome_vars <- seq_len(num_rep_meas)
  outcome_cor <- 0.2
  dt_ls <- rct_dgp_fun(
    num_part = 1000,
    num_rep_meas = num_rep_meas,
    outcome_vars = outcome_vars,
    outcome_cor = outcome_cor
  )
  output <- glmmTMB_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_covar <- VarCorr(output$fit)$cond$participant
  est_outcome_vars <- diag(est_covar)
  est_outcome_cor <- est_covar[1, 2] / sqrt(est_covar[1, 1] * est_covar[2, 2])
  var_names <- paste0("time", str_pad(seq_len(num_rep_meas), 2, "left", "0"))
  expect_equal(names(est_outcome_vars), var_names)
  expect_equal(as.numeric(est_outcome_vars), outcome_vars, tolerance = 0.1)
  expect_equal(est_outcome_cor, outcome_cor, tolerance = 0.1)
})

test_that("nlme wrapper estimates a treatment effect close to ground truth", {
  library(nlme)
  set.seed(815347)
  trt_effect <- 2
  dt_ls <- rct_dgp_fun(num_part = 1000, fixed_trt = trt_effect)
  output <- nlme_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_trt_effect <- as.numeric(coef(output$fit)["trt"])
  expect_equal(est_trt_effect, trt_effect, tolerance = 0.1)
})

test_that("nlme wrapper estimates a CSH parameters close to ground truth", {
  library(nlme)
  set.seed(81243)
  num_rep_meas <- 10
  outcome_vars <- seq_len(num_rep_meas)
  outcome_cor <- 0.2
  dt_ls <- rct_dgp_fun(
    num_part = 1000,
    num_rep_meas = num_rep_meas,
    outcome_vars = outcome_vars,
    outcome_cor = outcome_cor
  )
  output <- nlme_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_covar <- getVarCov(output$fit)
  est_outcome_vars <- diag(est_covar)
  est_outcome_cor <- est_covar[1, 2] / sqrt(est_covar[1, 1] * est_covar[2, 2])
  expect_equal(est_outcome_vars, outcome_vars, tolerance = 0.1)
  expect_equal(est_outcome_cor, outcome_cor, tolerance = 0.1)
})

test_that("PROC MIXED wrapper estimates a CSH parameters close to ground truth", {
  library(sasr)
  set.seed(71231)
  num_rep_meas <- 10
  outcome_vars <- seq_len(num_rep_meas)
  outcome_cor <- 0.2
  dt_ls <- rct_dgp_fun(
    num_part = 1000,
    num_rep_meas = num_rep_meas,
    outcome_vars = outcome_vars,
    outcome_cor = outcome_cor
  )
  output <- proc_mixed_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  est_outcome_vars <- output$fit$Estimate[1:num_rep_meas]
  est_outcome_cor <- output$fit$Estimate[num_rep_meas + 1]
  variance_names <- paste0(paste0("Var(", seq_len(num_rep_meas)), ")")
  expect_equal(output$fit$CovParm[1:num_rep_meas], variance_names)
  expect_equal(est_outcome_vars, outcome_vars, tolerance = 0.1)
  expect_equal(est_outcome_cor, outcome_cor, tolerance = 0.1)
})
