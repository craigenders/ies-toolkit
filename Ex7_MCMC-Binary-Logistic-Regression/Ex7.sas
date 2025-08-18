/* Bayes Linear Regression With a Categorical Outcome */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ id male hispanic riskgrp atrisk behsymp1 lrnprob1 read1 read2 read3 read9 readgrp9 stanread7 math1 math2 math3 math9 mathgrp9 stanmath7;

/* running regression */
proc logistic data = imps outest = estimates covout noprint;
model readgrp9(event='1') = read1 lrnprob1 behsymp1;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept read1 lrnprob1 behsymp1;
run;

