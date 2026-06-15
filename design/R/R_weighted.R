# Weighted MMRM and LS means-
library(mmrm)
library(nlme)
library(emmeans)

# Note: set VIS4 as reference level to match with SAS
fev_data <- within(fev_data, AVISIT <- relevel(AVISIT, ref = "VIS4"))

## mmrm package ----
formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
fit <- mmrm(formula, fev_data, weights = fev_data$WEIGHT)

sink(file = "R_weighted_mmrm.txt")
summary(fit)
cat("\n \n")
emmeans(fit, pairwise ~ ARMCD * AVISIT)
sink(file = NULL)

## nlme package ----
fev_data$INVW <- 1 / fev_data$WEIGHT
fit <- gls(
  model = FEV1 ~ RACE + SEX + ARMCD * AVISIT,
  data = fev_data,
  correlation = corSymm(form = ~ as.numeric(AVISIT) | USUBJID),
  weights = varComb(varIdent(form = ~ 1 | AVISIT), varFixed(~INVW)),
  control = glsControl(opt = "optim"),
  method = "REML",
  na.action = "na.omit"
)

sink(file = "R_weighted_nlme.txt")
summary(fit)
cat("\n \n")
emmeans(fit, pairwise ~ ARMCD * AVISIT)
sink(file = NULL)
