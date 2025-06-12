// Bayes Moderated Regression
clear

// read stacked data
infile imp id male hispanic riskgrp atrisk behsymp1 lrnprob1 read1 read2 read3 read9 read9grp stanread7 math1 math2 math3 math9 math9grp stanmath7 using "~/desktop/imps0.dat"

// recode missing data in original data (imp = 0)
recode male - stanmath7 (999 = .)

// convert to mi data
mi import flong, m(imp) id(id) imputed(male - stanmath7) clear

// compute means
egen read1_mean = mean(read1)
egen lrnprob1_mean = mean(lrnprob1)
egen atrisk_mean = mean(atrisk)
gen read1_cgm = read1 - read1_mean
gen lrnprob1_cgm = lrnprob1 - lrnprob1_mean
gen atrisk_cgm = atrisk - atrisk_mean

// analysis and pooling
mi estimate, cmdok: regress read9 read1_cgm lrnprob1_cgm atrisk_cgm c.read1_cgm#c.lrnprob1_cgm
