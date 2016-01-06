data choices;
	infile "/folders/myshortcuts/sas/choices.csv" delimiter=',' firstobs=2;
	input rabs samplesize offsetalpha rsgn smoothed plotstyle subjid weights detected logtime;
run;
data predgrid;
	infile "/folders/myshortcuts/sas/predgrid.csv" delimiter=',' firstobs=2;
	input rabs samplesize offsetalpha rsgn smoothed plotstyle subjid;
run;
data combined;
	set choices predgrid;
run;
ods trace on;
ods output ParameterEstimates=estimates;
proc glimmix data=combined;
	class subjid;
	model detected = rabs rsgn rabs*rsgn smoothed samplesize plotstyle plotstyle*rabs plotstyle*rsgn plotstyle*rabs*rsgn plotstyle*smoothed plotstyle*samplesize/ solution dist=binomial link=logit offset=offsetalpha;
	random subjid;
	weight weights;
	output out=modelfit pred(BLUP)=randomeffects resid=resids pred(NOBLUP)=fixedeffects;
run;
ods trace off;
ods trace on;
ods output SolutionF=timemodel;
proc mixed data=choices;
	class subjid;
	model logtime = rabs rsgn rabs*rsgn smoothed samplesize detected plotstyle plotstyle*rabs plotstyle*rsgn plotstyle*rabs*rsgn plotstyle*smoothed plotstyle*samplesize plotstyle*detected/ solution;
	random subjid;
	weight weights;
run;
ods trace off;
proc export data = WORK.ESTIMATES
	dbms = CSV
	label
	outfile = "/folders/myshortcuts/sas/PowerEstimates.csv"
	replace;
proc export data=WORK.MODELFIT 
	dbms = CSV
	label 
	outfile = "/folders/myshortcuts/sas/predfit.csv"
	replace;
proc export data = WORK.TIMEMODEL 
	dbms = CSV
	label
	outfile = "/folders/myshortcuts/sas/time.csv"
	replace;
run;

