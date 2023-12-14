%xpt2loc(libref = work, filespec = '/home/lil128/Project/mmrm/data-raw/fev.xpt');
ods output Tests1 = Tests1 Tests2 = Tests2 Tests3 = Tests3;

PROC MIXED DATA = fev cl method=reml;
  CLASS RACE(ref = '3') AVISIT(ref = '4') SEX(ref = '2') ARMCD(ref = '2') USUBJID;
  MODEL FEV1 = ARMCD*AVISIT ARMCD AVISIT / ddfm=satterthwaite htype=1,2,3;
  REPEATED AVISIT / subject=USUBJID type=ar(1);
RUN;

libname mm '/home/lil128/Project/mmrm/data-raw';
data mm.test1;
	set tests1;
data mm.test2;
	set tests2;
data mm.test3;
	set tests3;
run;