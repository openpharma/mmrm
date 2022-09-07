# Heterogeneous Toeplitz covariance structure ----
formula <- FEV1 ~ toeph(AVISIT | USUBJID)
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
      REPEATED AVISIT / subject=USUBJID type=TOEPH r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_toeph_ml.txt")

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
      REPEATED AVISIT / subject=USUBJID type=TOEPH r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_toeph_reml.txt")



# Grouped Heterogeneous Toeplitz covariance structure ----
formula <- FEV1 ~ toeph(AVISIT | ARMCD / USUBJID)
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
      REPEATED AVISIT / subject=USUBJID type=TOEPH r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr group=ARMCD;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_group_toeph_ml.txt")

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
      REPEATED AVISIT / subject=USUBJID type=TOEPH r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr group=ARMCD;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_group_toeph_reml.txt")
