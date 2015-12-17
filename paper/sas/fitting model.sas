data choices;
	infile "/folders/myshortcuts/sas/choices.csv" delimiter=',';
	input subjid detected correl rsgn rabs smoothed samplesize plotstyle logtime actuallocation attempts weights response0 offsetalpha;
ods trace on;
ods output ParameterEstimates=estimates;
proc glimmix;
	class subjid;
	model detected = rabs rsgn rabs*rsgn smoothed samplesize plotstyle plotstyle*rabs plotstyle*rsgn plotstyle*rabs*rsgn plotstyle*smoothed plotstyle*samplesize/ solution dist=binomial link=logit offset=offsetalpha;
	random subjid;
	weight weights;
proc export data = WORK.ESTIMATES
	dbms = CSV
	label
	outfile = "/folders/myshortcuts/sas/PowerEstimates.csv"
	replace;
run;
ods trace off;
