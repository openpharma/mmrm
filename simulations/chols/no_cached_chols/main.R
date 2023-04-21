# withr::with_libpaths("simulations/chols/no_cached_chols/library", remotes::install_github("openpharma/mmrm", ref = "main"))

library(mmrm, lib.loc = "simulations/chols/no_cached_chols/library")

formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID)

microbenchmark::microbenchmark(
  mmrm(formula, fev_data, reml = TRUE),
  times = 100L
)
