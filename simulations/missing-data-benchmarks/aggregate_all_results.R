library(googledrive)
library(dplyr)
drive_auth()

args <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  excludeReserved = TRUE
)

outname <- args$output
input_folder <- args$input

inputs <- list.files(input_folder, full.names = TRUE, pattern = ".*rds")
results <- lapply(inputs, readRDS)

all_results <- do.call(bind_rows, results)
saveRDS(all_results, outname)
