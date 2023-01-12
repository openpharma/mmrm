################################################################################
## Evaluator Function Tests
################################################################################

## load required libraries
library(here)
library(testthat)
library(stringr)

## load data-generating function and method wrapper functions
source(here("simulations/csh-benchmarks/R/dgp-functions.R"))
source(here("simulations/csh-benchmarks/R/method-functions.R"))
source(here("simulations/csh-benchmarks/R/eval-functions.R"))

## generate the data
set.seed(2612)
num_rep_meas <- 10
outcome_vars <- seq_len(num_rep_meas) / 2
outcome_cor <- 0.1
dt_ls <- rct_dgp_fun(
  num_part = 10000,
  num_rep_meas = num_rep_meas,
  outcome_vars = outcome_vars,
  outcome_cor = outcome_cor
)

## construct the true covariance matrix
true_covar_mat <- outer(sqrt(outcome_vars), sqrt(outcome_vars)) * outcome_cor
diag(true_covar_mat) <- outcome_vars

test_that("get_covar_mat() constructs accurate estimates from mmrm fits",
{
  library(mmrm)
  output <- mmrm_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  element_wise_error <- abs((get_covar_mat(output$fit) - true_covar_mat))
  above_element_wise_tol <- sum(element_wise_error > 0.1)
  expect_equal(above_element_wise_tol, 0)
})

test_that("get_covar_mat() constructs accurate estimates from glmmTMB fits",
{
  library(glmmTMB)
  output <- glmmTMB_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  element_wise_error <- abs((get_covar_mat(output$fit) - true_covar_mat))
  above_element_wise_tol <- sum(element_wise_error > 0.1)
  expect_equal(above_element_wise_tol, 0)
})

test_that("get_covar_mat() constructs accurate estimates from nlme fits",
{
  library(nlme)
  output <- nlme_wrapper_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  element_wise_error <- abs((get_covar_mat(output$fit) - true_covar_mat))
  above_element_wise_tol <- sum(element_wise_error > 0.1)
  expect_equal(above_element_wise_tol, 0)
})

test_that("get_covar_mat() constructs accurate estimates from PROC MIXED fits",
{
  library(nlme)
  output <- proc_mixed_fun(
    participant = dt_ls$participant,
    time = dt_ls$time,
    trt = dt_ls$trt,
    y = dt_ls$y
  )
  element_wise_error <- abs((get_covar_mat(output$fit) - true_covar_mat))
  above_element_wise_tol <- sum(element_wise_error > 0.1)
  expect_equal(above_element_wise_tol, 0)
})
