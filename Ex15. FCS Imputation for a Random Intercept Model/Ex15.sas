/* Two-Level Regression With Random Intercepts */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ school student condition teachexp eslpct hispanic male frlunch 
	lowach stanmath efficacypre efficacypst psolvepre psolvepst; 
run;

*/ compute cluster means;
proc means data = imps noprint;
var psolvepre hispanic frlunch;
by _imputation_ school;
output out = groupmeans (drop = _TYPE_ _FREQ_) mean = psolvepre_meanj hispanic_meanj frlunch_meanj;
run;

*/ add cluster means means to data and center;
data imps;
merge groupmeans imps;
by _imputation_ school;
psolvepre_cwc = psolvepre- psolvepre_meanj; 
hispanic_cwc = hispanic - hispanic_meanj; 
frlunch_cwc = frlunch - frlunch_meanj;
run;

/* compute grand means; */
proc means data = imps noprint;
var psolvepre_meanj hispanic_meanj frlunch_meanj;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = psolvepre_mean hispanic_mean frlunch_mean; 
run;

/* add grand means to data and center */
data imps;
if _N_ = 1 then set grandmeans;
set imps;
psolvepre_meanj = psolvepre_meanj - psolvepre_mean;
hispanic_meanj = hispanic_meanj - hispanic_mean;
frlunch_meanj = frlunch_meanj - frlunch_mean;
run;

/* running multi level model */
ods _all_ close;
proc mixed data = imps noclprint;
model psolvepst = psolvepre_cwc hispanic_cwc frlunch_cwc psolvepre_meanj hispanic_meanj frlunch_meanj condition / solution covb;
random intercept / subject = student type = un;
by _imputation_;
ods output SolutionF = estimates CovB = covb;
ods listing;
run;

proc mianalyze parms = estimates covb(effectvar = rowcol) = covb;
modeleffects Intercept psolvepre_cwc hispanic_cwc frlunch_cwc psolvepre_meanj hispanic_meanj frlunch_meanj condition;
run;