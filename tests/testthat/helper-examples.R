.tmb_formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
.mmrm_tmb_example <- h_mmrm_tmb(.tmb_formula, fev_data)
get_mmrm_tmb <- function() {
  .mmrm_tmb_example
}

.tmb_formula_rank_deficient <- FEV1 ~ SEX + SEX2 + us(AVISIT | USUBJID)
.mmrm_tmb_dat_rank_deficient <- cbind(fev_data, SEX2 = fev_data$SEX) # nolint
.mmrm_tmb_example_rk_deficient <- h_mmrm_tmb(
  .tmb_formula_rank_deficient,
  .mmrm_tmb_dat_rank_deficient
)
get_mmrm_tmb_rank_deficient <- function() {
  .mmrm_tmb_example_rk_deficient
}

.mmrm_formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
.mmrm_example <- mmrm(.mmrm_formula, fev_data)
get_mmrm <- function() {
  .mmrm_example
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
