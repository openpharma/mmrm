# Spatial Gaussian covariance structure ----
# Run from this directory. Produces fev.xpt + four SAS programs (sp_gau ML/REML
# x 1D/2D), then drives run sas to collect outputs matching the
# sas_sp_exp_*.txt format.

library(haven)

data <- fev_data
# SAS xport v5 wants character (not factor) and 8-char member names.
data$USUBJID <- as.character(data$USUBJID)
data$AVISIT <- as.character(data$AVISIT)
data$ARMCD <- as.character(data$ARMCD)
data$RACE <- as.character(data$RACE)
data$SEX <- as.character(data$SEX)
write_xpt(data, "fev.xpt", version = 5, name = "dat")

write_sas <- function(path, method, vars) {
  template <- sprintf(
    'libname xptfile xport "fev.xpt";
proc copy inlib=xptfile outlib=work; run;

PROC MIXED DATA = dat cl method=%s;
  CLASS USUBJID;
  MODEL FEV1 = / ddfm=satterthwaite solution chisq;
  REPEATED / subject=USUBJID type=sp(gau)(%s) rcorr;
RUN;
',
    method,
    vars
  )
  writeLines(template, path)
}

programs <- list(
  c("sas_sp_gau_ml.sas", "ml", "visitn"),
  c("sas_sp_gau2_ml.sas", "ml", "visitn visitn2"),
  c("sas_sp_gau_reml.sas", "reml", "visitn"),
  c("sas_sp_gau2_reml.sas", "reml", "visitn visitn2")
)
for (p in programs) write_sas(p[1], p[2], p[3])

# Kenward-Roger reference (default and first-order / linear).
writeLines(
  'libname xptfile xport "fev.xpt";
proc copy inlib=xptfile outlib=work; run;

PROC MIXED DATA = dat cl method=reml;
  CLASS USUBJID ARMCD;
  MODEL FEV1 = ARMCD / ddfm=kenwardroger solution chisq;
  REPEATED / subject=USUBJID type=sp(gau)(visitn visitn2) rcorr;
RUN;

PROC MIXED DATA = dat cl method=reml;
  CLASS USUBJID ARMCD;
  MODEL FEV1 = ARMCD / ddfm=kenwardroger(firstorder) solution chisq;
  REPEATED / subject=USUBJID type=sp(gau)(visitn visitn2) rcorr;
RUN;
',
  "sas_sp_gau_kr.sas"
)
