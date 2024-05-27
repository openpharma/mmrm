# Let's finally combine all the results together.

ocean_path <- "/ocean/harbour/CDTpractice/PR002/demo-biostats-workflow/dev/data/other/sabanesd"
sub_dirs <- c("extreme-miss", "high-miss", "low-miss", "no-miss")
r_sas_dirs <- c("n-600-R", "n-600-SAS")

combined_dirs <- expand.grid(sub_dirs, r_sas_dirs)
combined_dirs <- apply(combined_dirs, 1L, paste, collapse = "/")
combined_dirs <- file.path(ocean_path, combined_dirs, "formatted_fit_results.rds")

df_list <- lapply(combined_dirs, readRDS)
str(df_list, 1)

head(df_list[[5]])
