.tmb_formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
.mmrm_tmb_example <- h_mmrm_tmb(.tmb_formula, fev_data)
get_mmrm_tmb <- function() {
  .mmrm_tmb_example
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

map_to_cor <- function(x) {
  x / sqrt(1 + x^2)
}
