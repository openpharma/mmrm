* Homogeneous Toeplitz covariance structure ----;

** ML ----;
ODS RTF FILE="sas_toep_ml.rtf";
PROC MIXED DATA = fev_data cl method=ml;
  CLASS AVISIT(ref = 'VIS4') USUBJID;
  MODEL FEV1 = / ddfm=satterthwaite solution chisq;
  REPEATED AVISIT / subject=USUBJID type=TOEP r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
RUN;
ODS RTF CLOSE;

*** REML ----;
ODS RTF FILE="sas_toep_reml.rtf";
PROC MIXED DATA = fev_data cl method=reml;
  CLASS AVISIT(ref = 'VIS4') USUBJID;
  MODEL FEV1 = / ddfm=satterthwaite solution chisq;
  REPEATED AVISIT / subject=USUBJID type=TOEP r=1, 2, 3, 4, 5, 6, 7, 8, 9, 10 rcorr;
RUN;
ODS RTF CLOSE;
