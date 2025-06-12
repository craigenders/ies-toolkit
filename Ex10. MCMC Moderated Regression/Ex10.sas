/* Bayes Moderated Regression */
data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ id male hispanic riskgrp atrisk behsymp1 lrnprob1 read1 read2 read3 read9 read9grp stanread7 math1 math2 math3 math9 math9grp stanmath7;

/* compute grand means; */
proc means data = imps noprint;
var lrnprob1 read1 atrisk;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = lrnprob1_mean read1_mean atrisk_mean; 
run;

/* add grand means to data and center */
data imps;
if _N_ = 1 then set grandmeans;
set imps;
lrnprob1_cgm = lrnprob1 - lrnprob1_mean;
read1_cgm = read1 - read1_mean;
atrisk_cgm = atrisk - atrisk_mean;
read1_by_lrnprob1 = lrnprob1_cgm*read1_cgm
run;

/* running regression */
proc reg data = imps outest = estimates covout noprint;
model read9 = read1_cgm lrnprob1_cgm read1_by_lrnprob1 atrisk_cgm;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept read1_cgm lrnprob1_cgm read1_by_lrnprob1 atrisk_cgm;
run;