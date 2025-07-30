/*  FCS analysis for Paired-Samples Test */

data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ id condition male frlunch atrisk stanread efficacy anxiety mathpre mathpost;

/* creating change score */
data imps;
set imps;
change=mathpost-mathpre;
run;

/* running regression */
proc reg data = imps outest = estimates covout noprint;
model change = ;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept;
run;
