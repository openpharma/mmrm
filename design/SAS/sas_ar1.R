# Homogeneous auto-regressive covariance structure ----
formula <- FEV1 ~ ar1(AVISIT | USUBJID)
data <- fev_data

## ML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=ml;
      CLASS AVISIT(ref = 'VIS4') USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=AR(1) r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_ar1_ml.txt")

## REML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS AVISIT(ref = 'VIS4') USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=AR(1) r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_ar1_reml.txt")


# Homogeneous auto-regressive covariance structure ----
formula <- FEV1 ~ ar1(AVISIT | ARMCD / USUBJID)
data <- fev_data

## ML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=ml;
      CLASS AVISIT(ref = 'VIS4') USUBJID ARMCD;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=AR(1) r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr group = ARMCD;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_group_ar1_ml.txt")

## REML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS AVISIT(ref = 'VIS4') USUBJID ARMCD;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED AVISIT / subject=USUBJID type=AR(1) r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr group = ARMCD;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_group_ar1_reml.txt")
