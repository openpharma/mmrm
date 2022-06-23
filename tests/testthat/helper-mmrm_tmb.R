get_mmrm_tmb <- function() {
  formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
  data <- fev_data
  h_mmrm_tmb(formula, data)
}
