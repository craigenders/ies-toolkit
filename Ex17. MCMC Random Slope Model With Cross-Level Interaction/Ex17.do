// Two-Level Moderated Regression With Random Slopes
clear

// read stacked data
infile imp school student wave condition teachexp eslpct hispanic male frlunch lowach stanmath month month7 probsolve efficacy using "~/desktop/imps0.dat"

// recode missing data in original data (imp = 0)
recode condition - efficacy (999 = .)

// create unique row id within each data set
generate rownum = student*100 + wave

// convert to mi data
mi import flong, m(imp) id(rownum) imputed(condition - efficacy) clear

// center variables
summarize frlunch, meanonly
gen frlunch_cgm = frlunch - r(mean)

// analysis and pooling
mi estimate, cmdok: mixed probsolve month hispanic frlunch_cgm condition c.month#c.condition c.hispanic#c.condition || student: month, cov(unstructured) var
