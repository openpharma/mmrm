library(sasr)
library(mmrm)

df2sd(fev_data, "fev")

sas_result <- run_sas("
ods output diffs = diff;
PROC MIXED DATA = fev cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD / ddfm=kr solution chisq;
      REPEATED AVISIT / subject=USUBJID type=ar(1) r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;
")

result <- sd2df("diff")
cat(sas_result$LST, file = "design/KR/kr_ar1.txt")
write.csv(result, file = "design/KR/kr_ar1.csv")

fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT|USUBJID), data = fev_data)
kr(fit, contrast = matrix(c(0, 1), ncol = 1))

sas_result <- run_sas("
ods output diffs = diff;
PROC MIXED DATA = fev cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD / ddfm=kr solution chisq;
      REPEATED AVISIT / subject=USUBJID type=arh(1) r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;
")

result <- sd2df("diff")
cat(sas_result$LST, file = "design/KR/kr_arh1.txt")
write.csv(result, file = "design/KR/kr_arh1.csv")

fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT|USUBJID), data = fev_data)
kr(fit, contrast = matrix(c(0, 1), ncol = 1))


sas_result <- run_sas("
ods output diffs = diff;
PROC MIXED DATA = fev cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD / ddfm=kr solution chisq;
      REPEATED AVISIT / subject=USUBJID type=csh r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;
")

result <- sd2df("diff")
cat(sas_result$LST, file = "design/KR/kr_csh.txt")
write.csv(result, file = "design/KR/kr_csh.csv")

fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT|USUBJID), data = fev_data)
kr(fit, contrast = matrix(c(0, 1), ncol = 1))

sas_result <- run_sas("
ods output diffs = diff;
PROC MIXED DATA = fev cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD / ddfm=kr solution chisq;
      REPEATED AVISIT / subject=USUBJID type=cs r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;
")

result <- sd2df("diff")
cat(sas_result$LST, file = "design/KR/kr_cs.txt")
write.csv(result, file = "design/KR/kr_cs.csv")

fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT|USUBJID), data = fev_data)
kr(fit, contrast = matrix(c(0, 1), ncol = 1))


sas_result <- run_sas("
ods output diffs = diff;
PROC MIXED DATA = fev cl method=reml;
      CLASS RACE(ref = 'Asian') AVISIT(ref = 'VIS4') SEX(ref = 'Male') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = ARMCD / ddfm=kr solution chisq;
      REPEATED AVISIT / subject=USUBJID type=ante(1) r rcorr;
      LSMEANS ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
    RUN;
")

result <- sd2df("diff")
cat(sas_result$LST, file = "design/KR/kr_ante.txt")
write.csv(result, file = "design/KR/kr_ante.csv")

fit <- mmrm(FEV1 ~ ARMCD + ad(AVISIT|USUBJID), data = fev_data)
kr(fit, contrast = matrix(c(0, 1), ncol = 1))
