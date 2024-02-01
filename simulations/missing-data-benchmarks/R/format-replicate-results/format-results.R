# load required libraries
library(dplyr)
library(glmmTMB)
library(nlme)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(emmeans)

# load the required helper functions
source("R/format-replicate-results/helpers.R")
source("R/eval/helpers.R")

# load a fit_results.rds file
fit_results <- readRDS("R/format-replicate-results/fit_results_100.rds")

# format the results
formatted_fit_results_df <- fit_results %>%
  format_fit_results(missingness = "none", sample_size = 200)
