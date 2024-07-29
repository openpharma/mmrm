#! /usr/bin/env Rscript

args <- R.utils::commandArgs(asValues = TRUE, excludeReserved = TRUE)

missingness <- args$missingness
treatment_effect <- args$treatment_effect
reps <- args$reps
seed <- as.numeric(args$seed)
n <- as.numeric(args$n)
n_reps <- as.numeric(args$reps)
covar_rsp <- args$covar_rsp
covar_type <- args$covar_type
method <- args$method # "mmrm", "glmmtmb", "nlme", "sas"
reml <- args$reml # reml or ml
input_data <- args$input_data
output_file <- args$output_file

source("programs/analyze_data.R")

data <- arrow::read_parquet(input_data)

ret <- analyze_data(data, method, covar_rsp, covar_type, identical(reml, "reml"))

ret$missingness <- missingness
ret$n <- n
ret$treatment_effect <- treatment_effect
ret$seed <- seed
ret$reml <- reml
ret$covar_rsp <- covar_rsp

saveRDS(ret, output_file)
