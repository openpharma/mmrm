# Spatial exponential covariance structure ----
formula <- FEV1 ~ sp_exp(VISITN | USUBJID)
formula2 <- FEV1 ~ sp_exp(VISITN, VISITN2 | USUBJID)
data <- fev_data
data$VISITN2 <- data$VISITN + 1

## ML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=ml;
      CLASS USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED / subject=USUBJID type=sp(exp)(visitn) rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_sp_exp_ml.txt")


sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=ml;
      CLASS USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED / subject=USUBJID type=sp(exp)(visitn visitn2) rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_sp_exp2_ml.txt")

## REML ----
sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED / subject=USUBJID type=sp(exp)(visitn) rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_sp_exp_reml.txt")

sascode <- list(
  # "show" = "
  #   PROC CONTENTS DATA = ana.dat;
  #   RUN;
  # "
  "test" = "
    PROC MIXED DATA = ana.dat cl method=reml;
      CLASS USUBJID;
      MODEL FEV1 = / ddfm=satterthwaite solution chisq;
      REPEATED / subject=USUBJID type=sp(exp)(visitn visitn2) rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_sp_exp2_reml.txt")
