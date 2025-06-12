// Bayes Regression With Categorical Predictors
clear
// read stacked data
infile imp id condition male frlunch atrisk stanread efficacy anxiety mathpre mathpost using "~/desktop/imps0.dat"

// recode missing data in original data (imp = 0)
recode condition - mathpost (999 = .)

// convert to mi data
mi import flong, m(imp) id(id) imputed(condition - mathpost) clear

// analysis and pooling
mi estimate, cmdok: regress mathpost condition frlunch efficacy mathpre
