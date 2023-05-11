################################################################################
# Plot the Simulation Results for All DGPs
################################################################################

# load required libraries
library(simChef)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# source the R plotting scripts
sim_functions_files <- list.files(
  c("R/viz"),
  pattern = "*.R$", full.names = TRUE, ignore.case = TRUE
)
sapply(sim_functions_files, source)
source("R/plot-results/helpers.R")

# find all of the folders in the results subdirectory
results_dirs <- list.dirs("results", full.names = TRUE, recursive = TRUE)

# extract paths containing simulation results
results_dirs <- results_dirs[str_detect(results_dirs, "n-[0-9]00$")]

# generate and save all of the simulation result plots
sapply(results_dirs, save_plots)
