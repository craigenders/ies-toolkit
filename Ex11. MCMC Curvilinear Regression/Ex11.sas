/* Bayes Curvilinear Regression */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ id condition male frlunch atrisk stanread efficacy anxiety mathpre mathpost;

/* creating categorical variable */
frlunch2 = 0;
if frlunch = 0 then frlunch2 = 1;
efficacy2 = 0;
if efficacy = 2 then frlunch2 = 1;
efficacy3 = 0;
if efficacy = 3 then frlunch3 = 1;
efficacy4 = 0;
if efficacy = 4 then frlunch4 = 1;
efficacy5 = 0;
if efficacy = 5 then frlunch5 = 1;
efficacy6 = 0;
if efficacy = 6 then frlunch6 = 1;
run;


/* compute grand means; */
proc means data = imps noprint;
var anxiety;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = anxietyMean; 
run;

/* add grand means to data and center */
data imps;
if _N_ = 1 then set grandmeans;
set imps;
anxietycent = anxiety - anxietyMean;
run;

/* creating squared term */
data imps;
set imps;
anxietycent_squared=anxiety**2;
run;

/* running regression */
proc reg data = imps outest = estimates covout noprint;
model mathpost = anxietycent anxietycent_squared frlunch efficacy mathpre;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept anxietycent anxietycent_squared frlunch efficacy mathpre;;
run;