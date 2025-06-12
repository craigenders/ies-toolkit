// Bayes Regression With Nominal Predictors
clear
// read stacked data
infile imp id male hispanic riskgrp atrisk behsymp1 lrnprob1 read1 read2 read3 read9 read9grp stanread7 math1 math2 math3 math9 math9grp stanmath7 using "~/desktop/imps0.dat"

// recode missing data in original data (imp = 0)
recode male - stanmath7 (999 = .)

// convert to mi data
mi import flong, m(imp) id(id) imputed(male - stanmath7) clear

// analysis and pooling
mi estimate, cmdok: regress read9 read1 lrnprob1 behsymp1 i.riskgrp
