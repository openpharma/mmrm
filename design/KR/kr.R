library(sasr)
library(mmrm)
df2sd(fev_data, "fev")

sas_code <- function(ddfm, covtype) {
  sprintf("ods output diffs = diff;
    PROC MIXED DATA = fev cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD / ddfm=%s solution chisq;
      REPEATED AVISIT / subject=USUBJID type=%s r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;", ddfm, covtype)
}


sas_wrapper <- function(ddfm, covtype, result_name) {
  sas_result <- run_sas(sas_code(ddfm, covtype))
  result <- sd2df("diff")
  cat(sas_result$LST, file = sprintf("design/KR/%s.txt", result_name))
  write.csv(result, file = sprintf("design/KR/%s.csv", result_name))
}

sas_wrapper("kr", "ar(1)", "kr_ar1")
sas_wrapper("kr(linear)", "ar(1)", "kr1_ar1")
sas_wrapper("kr", "arh(1)", "kr_arh1")
sas_wrapper("kr(linear)", "arh(1)", "kr1_arh1")
sas_wrapper("kr", "cs", "kr_cs")
sas_wrapper("kr(linear)", "cs", "kr1_cs")
sas_wrapper("kr", "csh", "kr_csh")
sas_wrapper("kr(linear)", "csh", "kr1_csh")
sas_wrapper("kr", "ante(1)", "kr_adh")
sas_wrapper("kr(linear)", "ante(1)", "kr1_adh")
sas_wrapper("kr", "toep", "kr_toep")
sas_wrapper("kr(linear)", "toep", "kr1_toep")
sas_wrapper("kr", "toeph", "kr_toeph")
sas_wrapper("kr(linear)", "toeph", "kr1_toeph")
sas_wrapper("kr", "un", "kr_us")
sas_wrapper("kr(linear)", "un", "kr1_us")
sas_wrapper("kr", "sp(exp)(VISITN VISITN2)", "kr1_spexp")
sas_wrapper("kr(linear)", "sp(exp)(VISITN VISITN2)", "kr_spexp")
