# load required libraries
library(simChef)
library(checkmate)
library(testthat)
library(mmrm)
library(glmmTMB)
library(nlme)
library(sasr)
library(stringr)
library(dplyr)
library(tidyr)
library(stringr)
library(emmeans)

# flag whether SAS tests should be run
run_sas_tests <- FALSE

# source the R scripts
sim_functions_files <- list.files(
  c("R/dgp", "R/method", "R/eval", "R/viz"),
  pattern = "*.R$", full.names = TRUE, ignore.case = TRUE
)
sapply(sim_functions_files, source)

test_sim_dir()
