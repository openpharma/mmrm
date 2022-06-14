# Real MMRM ----
formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
data <- fev_data

## ML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=ml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS1') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD AVISIT ARMCD*AVISIT RACE SEX / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=un r rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_log.txt")

## REML ----
sascode_reml <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS1') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD AVISIT ARMCD*AVISIT RACE SEX / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=un r rcorr;
    RUN;
      "
)
result_reml <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode_reml)
result_reml$test$sas_log
writeLines(result_reml$test$sas_out, con = "sas_log_reml.txt")

# Simple model ----
formula <- FEV1 ~ us(AVISIT | USUBJID)
data <- fev_data

## ML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=ml;
      CLASS AVISIT(ref = 'VIS1') USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=un r rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_log_simple.txt")

## REML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS AVISIT(ref = 'VIS1') USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=un r rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_log_simple_reml.txt")
