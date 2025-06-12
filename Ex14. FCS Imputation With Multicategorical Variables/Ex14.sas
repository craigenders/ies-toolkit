/* FCS for Regression With Categorical Predictors */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ school student condition teachexp eslpct ethnic male frlunch 
	lowach stanmath efficacypre efficacypst psolvepre psolvepst; 
	
/* creating categorical variable */
riskgrp2 = 0;
if riskgrp = 2 then riskgrp2 = 1;
riskgrp3 = 0;
if riskgrp = 3 then riskgrp3 = 1;
run;

/* running regression */
proc reg data = imps outest = estimates covout noprint;
model read9 = read1 lrnprob1 behsymp1 riskgrp;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept read1 lrnprob1 behsymp1 riskgrp;
run;