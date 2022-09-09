# Spatial exponential covariance structure ----
formula <- FEV1 ~ sp_exp(TIME | USUBJID)
data <- fev_data
data$TIME <- data$VISITN

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
      REPEATED / subject=USUBJID type=sp(exp)(time) rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_gp_exp_ml.txt")

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
      REPEATED / subject=USUBJID type=sp(exp)(time) rcorr;
    RUN;
      "
)
result <- r2stream::bee_sas(data = list("dat" = data), sascode = sascode)
result$test$sas_log
writeLines(result$test$sas_out, con = "sas_gp_exp_reml.txt")
