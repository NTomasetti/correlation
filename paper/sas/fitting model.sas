data choices;
	infile "/folders/myshortcuts/sas/choices.csv" delimiter=',';
	input subjid detected correl rsgn rabs smoothed samplesize plotstyle logtime actuallocation attempts weights response0 offsetalpha;
proc glimmix;
	model detected = rabs rsgn rabs*rsgn smoothed samplesize plotstyle plotstyle*rabs plotstyle*rsgn plotstyle*rabs*rsgn plotstyle*smoothed plotstyle*samplesize/ solution dist=binomial link=logit;
	random subjid;
	weight weights;
	output out = modelfit;
run;

