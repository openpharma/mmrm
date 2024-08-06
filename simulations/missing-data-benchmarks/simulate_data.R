args <- R.utils::commandArgs(asValues = TRUE, excludeReserved = TRUE)

seed <- as.numeric(args$seed)
missingness <- args$missingness # "no", "low", "high", "extreme"
treatment_effect <- args$treatment_effect # "no", "small", "mod"
xpt_out <- args$xpt
parquet_out <- args$parquet
n <- as.numeric(args$n)
nrep <- as.numeric(args$rep)

# source the R scripts
source("programs/generate_data.R")

# generate the simulation data
set.seed(seed + n)

data <- generate_data(
  n_sample_size = n,
  n_rep = nrep,
  n_visits = 10,
  missing_level = missingness,
  trt_visit_coef = switch(treatment_effect,
    no = 0,
    small = 0.25,
    mod = 0.5
  ),
)

arrow::write_parquet(data, parquet_out)

haven::write_xpt(
  data,
  xpt_out,
  name = "simdata",
  version = 5
)
