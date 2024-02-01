library(sasr)
df2sd(fev_data, "data")

run_sas_mmrm <- function(method = c("ml", "reml"), cov = "UN") {
  sascode <- sprintf("
      PROC MIXED DATA = data cl method=%s;
        CLASS ARMCD(ref = 'PBO') AVISIT(ref = 'VIS4') SEX(ref='Male') USUBJID;
        MODEL FEV1 = ARMCD SEX/ ddfm=satterthwaite chisq solution cl ;
        REPEATED AVISIT / subject=USUBJID type=%s r rcorr;
      RUN;", method, cov)
  run_sas(sascode)
}

result <- run_sas_mmrm("ml", "un")
cat(result$LST, file = "design/SAS/sas_coef_ci_ml_un.txt")

result <- run_sas_mmrm("reml", "un")
cat(result$LST, file = "design/SAS/sas_coef_ci_reml_un.txt")

result <- run_sas_mmrm("ml", "ar(1)")
cat(result$LST, file = "design/SAS/sas_coef_ci_ml_ar1.txt")

result <- run_sas_mmrm("reml", "ar(1)")
cat(result$LST, file = "design/SAS/sas_coef_ci_reml_ar1.txt")
