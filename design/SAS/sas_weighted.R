# Weighted MMRM -
formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
fit <- mmrm(formula, fev_data, weights = fev_data$WEIGHT)

## REML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = RACE SEX ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=un r rcorr;
      WEIGHT WEIGHT;
      LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = fev_data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_weighted.txt")
