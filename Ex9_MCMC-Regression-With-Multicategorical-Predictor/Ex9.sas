data imps;
infile '/folders/myfolders/imps.dat' delimiter = " ";
input _imputation_ id male hispanic riskgrp atrisk behsymp1 lrnprob1 read1 read2 read3 read9 read9grp stanread7 math1 math2 math3 math9 math9grp stanmath7;
riskgrp2 = 0;
riskgrp3 = 0;
if riskgrp = 2 then riskgrp2 = 1;
if riskgrp = 3 then riskgrp3 = 1;
run;

proc reg data = imps outest = estimates covout noprint;
model read9 = read1 lrnprob1 behsymp1 riskgrp2 riskgrp3;
by _imputation_;
run;

proc mianalyze data = estimates;
modeleffects Intercept read1 lrnprob1 behsymp1 riskgrp2 riskgrp3;
run;