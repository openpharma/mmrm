# withr::with_libpaths("simulations/chols/cached_chols/library", remotes::install_github("openpharma/mmrm", ref = "232_cholesky"))
library(mmrm, lib.loc = "simulations/chols/cached_chols/library")

formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + ad(AVISIT | USUBJID)

microbenchmark::microbenchmark(
  mmrm(formula, fev_data, reml = TRUE),
  times = 100L
)
