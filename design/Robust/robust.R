library(sasr)
library(mmrm)
df2sd(fev_data, "fev")

sas_code <- function(covtype, weight) {
  sprintf("ods output diffs = diff covb = vcov;
    PROC MIXED DATA = fev empirical cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD /solution chisq covb;
      REPEATED AVISIT / subject=USUBJID type=%s r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
      %s
    RUN;", covtype, if (weight) "WEIGHT WEIGHT;" else "")
}


sas_wrapper <- function(ddfm, result_name, weight = FALSE) {
  sas_result <- run_sas(sas_code(ddfm, weight))
  result_lsmean <- sd2df("diff")
  result_vcov <- sd2df("vcov")
  cat(sas_result$LST, file = sprintf("design/Robust/%s.txt", result_name))
  write.csv(result_lsmean, file = sprintf("design/Robust/%s_lsmean.csv", result_name))
  result_vcov$ARMCD <- as.character(result_vcov$ARMCD)
  write.csv(result_vcov, file = sprintf("design/Robust/%s_covb.csv", result_name))
}

sas_wrapper("ar(1)", "empirical_ar1")
sas_wrapper("arh(1)", "empirical_arh1")
sas_wrapper("cs", "empirical_cs")
sas_wrapper("csh", "empirical_csh")
sas_wrapper("ante(1)", "empirical_adh")
sas_wrapper("toep", "empirical_toep")
sas_wrapper("toeph", "empirical_toeph")
sas_wrapper("un", "empirical_us")
sas_wrapper("sp(exp)(VISITN VISITN2)", "empirical_spexp")

sas_wrapper("ar(1)", "empirical_ar1_weighted", TRUE)
sas_wrapper("arh(1)", "empirical_arh1_weighted", TRUE)
sas_wrapper("cs", "empirical_cs_weighted", TRUE)
sas_wrapper("csh", "empirical_csh_weighted", TRUE)
sas_wrapper("ante(1)", "empirical_adh_weighted", TRUE)
sas_wrapper("toep", "empirical_toep_weighted", TRUE)
sas_wrapper("toeph", "empirical_toeph_weighted", TRUE)
sas_wrapper("un", "empirical_us_weighted", TRUE)
sas_wrapper("sp(exp)(VISITN VISITN2)", "empirical_spexp_weighted", TRUE)
