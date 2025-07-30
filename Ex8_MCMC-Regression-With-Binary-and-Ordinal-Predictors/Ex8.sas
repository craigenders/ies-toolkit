/* Bayes Regression With Categorical Predictors */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ id condition male frlunch atrisk stanread efficacy anxiety mathpre mathpost;

/* creating categorical variable */
condition2 = 0;
if condition = 0 then condition2 = 1;
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

/* running regression */
proc reg data = imps outest = estimates covout noprint;
model mathpost = condition frlunch efficacy mathpre;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept condition frlunch efficacy mathpre;
run;