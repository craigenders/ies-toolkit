// Bayes Curvilinear Regression
clear

// read stacked data
infile imp id condition male frlunch atrisk stanread efficacy anxiety mathpre mathpost using "~/desktop/imps0.dat"

// recode missing data in original data (imp = 0)
recode condition - mathpost (999 = .)

// convert to mi data
mi import flong, m(imp) id(id) imputed(condition - mathpost) clear

// creating a squared anxiety term
generate anxiety_squared = anxiety^2

// analysis and pooling
mi estimate, cmdok: regress mathpost anxiety anxiety_squared frlunch efficacy mathpre
