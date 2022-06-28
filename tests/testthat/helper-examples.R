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
