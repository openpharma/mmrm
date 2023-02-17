# load required libraries
library(simChef)
library(testthat)

# source the R scripts
sim_functions_files = list.files(
  c("R/dgp", "R/method", "R/eval", "R/viz"),
  pattern = "*.R$", full.names = TRUE, ignore.case = TRUE
)
sapply(sim_functions_files, source)

test_sim_dir()
