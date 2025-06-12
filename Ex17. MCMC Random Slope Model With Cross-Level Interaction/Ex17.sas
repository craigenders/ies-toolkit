/* Two-Level Moderated Regression With Random Slopes */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ school student wave condition teachexp eslpct ethnic male frlunch 
     lowach stanmath month month7 probsolve efficacy;
month_by_condition = month*condition;
month_by_hispanic = month*hispanic;
run;

/* compute grand means; */
proc means data = imps noprint;
var frlunch;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = frlunch_mean; 
run;

/* add grand means to data and center */
data imps;
if _N_ = 1 then set grandmeans;
set imps;
frlunch_cgm = frlunch - frlunch_mean;
run;

/* running multi level model */
ods _all_ close;
proc mixed data = imps noclprint;
model probsolve = month frlunch_cgm hispanic condition month_by_condition month_by_hispanic / solution covb;
random intercept month / subject = student type = un;
by _imputation_;
ods output SolutionF = estimates CovB = covb;
ods listing;
run;

proc mianalyze parms = estimates covb(effectvar = rowcol) = covb;
modeleffects Intercept month frlunch_cgm hispanic condition month_by_condition month_by_hispanic;
run;