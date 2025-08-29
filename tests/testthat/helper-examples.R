# We need to make sure to have deterministic hash to run tests.
TMB::config(tmbad_deterministic_hash = 1, DLL = "mmrm")

.tmb_formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
.mmrm_tmb_example <- fit_mmrm(
  .tmb_formula,
  fev_data,
  weights = rep(1, nrow(fev_data))
)
get_mmrm_tmb <- function() {
  .mmrm_tmb_example
}

.mmrm_tmb_trans <- fit_mmrm(
  FEV1 ~ log(FEV1_BL) + ar1(AVISIT | USUBJID),
  data = fev_data,
  weights = rep(1, nrow(fev_data))
)

get_mmrm_transformed <- function() {
  .mmrm_tmb_trans
}

.mmrm_trans <- mmrm(
  FEV1 ~ log(FEV1_BL) + ARMCD * AVISIT + ar1(AVISIT | USUBJID),
  data = fev_data
)

get_mmrm_trans <- function() {
  .mmrm_trans
}

.mmrm_multi_resp <- mmrm(
  FEV1 + log(WEIGHT) ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

get_mmrm_multi_resp <- function() {
  .mmrm_multi_resp
}

.tmb_formula_rank_deficient <- FEV1 ~ SEX + SEX2 + us(AVISIT | USUBJID)
.mmrm_tmb_dat_rank_deficient <- cbind(fev_data, SEX2 = fev_data$SEX) # nolint
.mmrm_tmb_example_rk_deficient <- fit_mmrm(
  .tmb_formula_rank_deficient,
  .mmrm_tmb_dat_rank_deficient,
  weights = rep(1, nrow(fev_data))
)
get_mmrm_tmb_rank_deficient <- function() {
  .mmrm_tmb_example_rk_deficient
}

.mmrm_formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
.mmrm_example <- mmrm(.mmrm_formula, fev_data)
get_mmrm <- function() {
  .mmrm_example
}

set.seed(123, kind = "Mersenne-Twister")
.mmrm_weights <- rpois(nrow(fev_data), lambda = 5) + 1
.mmrm_weighted_example <- mmrm(.mmrm_formula, fev_data, weights = .mmrm_weights)
get_mmrm_weighted <- function() {
  .mmrm_weighted_example
}

.mmrm_formula_rank_deficient <- FEV1 ~
  RACE + SEX + SEX2 + ARMCD * AVISIT + us(AVISIT | USUBJID)
.mmrm_dat_rank_deficient <- cbind(fev_data, SEX2 = fev_data$SEX) # nolint
.mmrm_example_rank_deficient <- mmrm(
  .mmrm_formula_rank_deficient,
  .mmrm_dat_rank_deficient
)
get_mmrm_rank_deficient <- function() {
  .mmrm_example_rank_deficient
}

.mmrm_group_formula <- FEV1 ~ ARMCD + us(AVISIT | ARMCD / USUBJID)
.mmrm_grouped <- mmrm(.mmrm_group_formula, data = fev_data)
get_mmrm_group <- function() {
  .mmrm_grouped
}

.mmrm_spatial_formula <- FEV1 ~ ARMCD + sp_exp(VISITN | USUBJID)
.mmrm_spatial <- mmrm(.mmrm_spatial_formula, data = fev_data)
get_mmrm_spatial <- function() {
  .mmrm_spatial
}

.mmrm_kr_formula <- FEV1 ~ ARMCD + ar1(AVISIT | USUBJID)
.mmrm_kr <- mmrm(.mmrm_kr_formula, data = fev_data, method = "Kenward-Roger")
get_mmrm_kr <- function() {
  .mmrm_kr
}

.mmrm_krl <- mmrm(
  .mmrm_kr_formula,
  data = fev_data,
  method = "Kenward-Roger",
  vcov = "Kenward-Roger-Linear"
)
get_mmrm_krl <- function() {
  .mmrm_krl
}

.mmrm_emp <- mmrm(
  .mmrm_kr_formula,
  data = fev_data,
  vcov = "Empirical",
  method = "Residual"
)
get_mmrm_emp <- function() {
  .mmrm_emp
}

.mmrm_bw <- mmrm(.mmrm_kr_formula, data = fev_data, method = "Between-Within")
get_mmrm_bw <- function() {
  .mmrm_bw
}

.mmrm_jackknife <- mmrm(
  .mmrm_kr_formula,
  data = fev_data,
  vcov = "Empirical-Jackknife",
  method = "Residual"
)
get_mmrm_jack <- function() {
  .mmrm_jackknife
}

.mmrm_brl <- mmrm(
  .mmrm_kr_formula,
  data = fev_data,
  vcov = "Empirical-Bias-Reduced",
  method = "Residual"
)
get_mmrm_brl <- function() {
  .mmrm_brl
}

.alt_fev_data <- fev_data
.alt_fev_data$FEV1 <-
  ifelse(is.na(fev_data$FEV1), fev_data$FEV1_BL, fev_data$FEV1) + 0.1
.alt_fev_data$FEV1[1:2] <- fev_data$FEV1[1:2]
.alt_fev_data$AVISIT2 <- .alt_fev_data$AVISIT
.alt_fev_data$USUBJID2 <- .alt_fev_data$USUBJID
.alt_fev_data$ARMCD2 <- .alt_fev_data$ARMCD

.mmrm_alt_data <- mmrm(.mmrm_formula, .alt_fev_data)
get_mmrm_alt_data <- function() {
  .mmrm_alt_data
}

.mmrm_formula_alt_visit <- FEV1 ~ ARMCD + us(AVISIT2 | ARMCD / USUBJID)
.mmrm_alt_visit <- mmrm(.mmrm_formula_alt_visit, .alt_fev_data)
get_mmrm_alt_visit <- function() {
  .mmrm_alt_visit
}

.mmrm_formula_alt_subj <- FEV1 ~ ARMCD + us(AVISIT | ARMCD / USUBJID2)
.mmrm_alt_subj <- mmrm(.mmrm_formula_alt_subj, .alt_fev_data)
get_mmrm_alt_subj <- function() {
  .mmrm_alt_subj
}

.mmrm_formula_alt_group <- FEV1 ~ ARMCD + us(AVISIT | ARMCD2 / USUBJID)
.mmrm_alt_group <- mmrm(.mmrm_formula_alt_group, .alt_fev_data)
get_mmrm_alt_group <- function() {
  .mmrm_alt_group
}

.smaller_fev_data <- fev_data[fev_data$AVISIT != "VIS4", ]
.mmrm_smaller_data <- mmrm(.mmrm_formula, .smaller_fev_data)
get_mmrm_smaller_data <- function() {
  .mmrm_smaller_data
}

.mmrm_formula_cs <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + cs(AVISIT | USUBJID)
.mmrm_example_cs <- mmrm(.mmrm_formula_cs, fev_data)
get_mmrm_cs <- function() {
  .mmrm_example_cs
}


square_matrix <- function(values_by_row) {
  n <- length(values_by_row)
  size <- sqrt(n)
  assert_integerish(size)
  size <- floor(size)
  matrix(data = values_by_row, nrow = size, ncol = size, byrow = TRUE)
}

map_to_cor <- function(theta) {
  theta / sqrt(1 + theta^2)
}

map_to_theta <- function(rho) {
  sign(rho) * sqrt(rho^2 / (1 - rho^2))
}

map_to_cs_cor <- function(theta, n_visits) {
  a <- 1 / (n_visits - 1)
  plogis(theta) * (1 + a) - a
}

map_to_cs_theta <- function(rho, n_visits) {
  a <- 1 / (n_visits - 1)
  qlogis((rho + a) / (1 + a))
}

expect_snapshot_tolerance <- function(
  x,
  style = "deparse",
  tolerance = 1e-4,
  ...
) {
  testthat::expect_snapshot_value(x, style = style, tolerance = tolerance, ...)
}

silly_optimizer <- function(
  par,
  objective,
  gr,
  value_add,
  message,
  control,
  ...
) {
  result <- par + value_add
  list(
    par = result,
    objective = objective(result),
    convergence = 0,
    message = message
  )
}

fail_optimizer <- function(par, objective, gr, message, control, ...) {
  result <- par
  list(
    par = result,
    objective = objective(result),
    convergence = 1,
    message = message
  )
}
