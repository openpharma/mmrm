.tmb_formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
.mmrm_tmb_example <- fit_mmrm(.tmb_formula, fev_data, weights = rep(1, nrow(fev_data)))
get_mmrm_tmb <- function() {
  .mmrm_tmb_example
}

.mmrm_tmb_trans <- fit_mmrm(
  FEV1 ~ log(FEV1_BL) + ar1(AVISIT | USUBJID),
  data = fev_data, weights = rep(1, nrow(fev_data))
)

get_mmrm_transformed <- function() {
  .mmrm_tmb_trans
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

.mmrm_formula_rank_deficient <- FEV1 ~ RACE + SEX + SEX2 + ARMCD * AVISIT + us(AVISIT | USUBJID)
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

.mmrm_krl <- mmrm(.mmrm_kr_formula, data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
get_mmrm_krl <- function() {
  .mmrm_krl
}

.mmrm_emp <- mmrm(.mmrm_kr_formula, data = fev_data, vcov = "Empirical", method = "Residual")
get_mmrm_emp <- function() {
  .mmrm_emp
}

.mmrm_bw <- mmrm(.mmrm_kr_formula, data = fev_data, method = "Between-Within")
get_mmrm_bw <- function() {
  .mmrm_bw
}

.mmrm_jackknife <- mmrm(.mmrm_kr_formula, data = fev_data, vcov = "Empirical-Jackknife", method = "Residual")
get_mmrm_jack <- function() {
  .mmrm_jackknife
}

.mmrm_brl <- mmrm(.mmrm_kr_formula, data = fev_data, vcov = "Empirical-Bias-Reduced", method = "Residual")
get_mmrm_brl <- function() {
  .mmrm_brl
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

expect_snapshot_tolerance <- function(x, style = "deparse", tolerance = 1e-4, ...) {
  testthat::expect_snapshot_value(x, style = style, tolerance = tolerance, ...)
}

silly_optimizer <- function(par, objective, gr, value_add, message, control, ...) {
  result <- par + value_add
  list(
    par = result,
    objective = objective(result),
    convergence = 0,
    message = message
  )
}
