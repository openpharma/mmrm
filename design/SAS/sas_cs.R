# Homogeneous compound symmetry covariance structure ----
formula <- FEV1 ~ cs(AVISIT | USUBJID)
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
      REPEATED AVISIT / subject=USUBJID type=CS r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_cs_ml.txt")

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
      REPEATED AVISIT / subject=USUBJID type=CS r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_cs_reml.txt")

# Homogeneous compound symmetry covariance structure ----
formula <- FEV1 ~ cs(AVISIT | ARMCD / USUBJID)
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
      REPEATED AVISIT / subject=USUBJID type=CS r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr group=ARMCD;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_group_cs_ml.txt")

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
      REPEATED AVISIT / subject=USUBJID type=CS r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr group=ARMCD;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_group_cs_reml.txt")
