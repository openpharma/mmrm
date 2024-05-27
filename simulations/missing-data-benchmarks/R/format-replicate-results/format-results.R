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
# This is just an example how to use these functions.

source("R/format-replicate-results/helpers.R")
source("R/eval/helpers.R")

# load a fit_results.rds file
fit_results <- readRDS("R/format-replicate-results/fit_results_test.rds")

# format the results
formatted_fit_results_df <- format_fit_results(fit_results)
saveRDS(formatted_fit_results_df, file = "R/format-replicate-results/df_test.rds")

# formatted_fit_results_df <- readRDS("df_test.rds")
head(formatted_fit_results_df)

# to obtain the estimated covariance matrices:
formatted_fit_results_df$covmat_estimates[1:2]

# to obtain the emmeans by visit:
formatted_fit_results_df |>
  select(- covmat_estimates, - fit_time, - converged) |>
  unnest(cols = emmeans_output)

