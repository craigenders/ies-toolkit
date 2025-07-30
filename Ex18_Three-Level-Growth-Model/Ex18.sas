/* Three-Level Regression with a Cross-Level Interaction */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ school student wave condition teachexp eslpct ethnic male frlunch 
     lowach stanmath month0 month7 probsolve efficacy;
month_by_condition = month7*condition;
run;

/* creating categorical variable */
data imps;
set imps;
male2 = 0;
if male = 0 then male2 = 1;
condition2 = 0;
if condition = 0 then condition2 = 1;
frlunch2 = 0;
if frlunch = 0 then frlunch2 = 1;
run;

/* compute grand means; */
proc means data = imps noprint;
var male frlunch teachexp;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = maleMean frlunchMean teachexpMean; 
run;

/* add grand means to data and center */
data imps;
if _N_ = 1 then set grandmeans;
set imps;
malecent = male - maleMean;
frlunchcent = frlunch - frlunchMean;
teachexpcent = teachexp - teachexpMean;
run;


/* running multi level model */
ods _all_ close;
proc mixed data = imps noclprint;
model probsolve = month7 malecent frlunchcent teachexpcent condition month_by_condition / solution covb;
random intercept month7 / subject = student type = un;
by _imputation_;
ods output SolutionF = estimates CovB = covb;
ods listing;
run;

proc mianalyze parms = estimates covb(effectvar = rowcol) = covb;
modeleffects Intercept month7 malecent frlunchcent teachexpcent condition month_by_condition;
run;