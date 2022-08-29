* Weighted MMRM ----;

** ML ----;

ODS RTF FILE="&datpath\sas_weighted_mmrm_ml.rtf";

proc mixed data = fev_data cl method = ml;
class AVISIT(ref = 'VIS4') USUBJID;
model FEV1 = / ddfm=satterthwaite solution chisq;
repeated AVISIT / subject=USUBJID type=ante(1) r=1,2,3,4,5, 6, 7, 8, 9, 10 rcorr;
weight WEIGHT;
run;

ODS RTF CLOSE;


*** REML ----;

ODS RTF FILE="&datpath\sas_weighted_mmrm_reml.rtf";

proc mixed data = fev_data cl method = reml;
class AVISIT(ref = 'VIS4') USUBJID;
model FEV1 = / ddfm=satterthwaite solution chisq;
repeated AVISIT / subject=USUBJID type=ante(1) r=1,2,3,4,5, 6, 7, 8, 9, 10 rcorr;
weight WEIGHT;
run;

ODS RTF CLOSE;
