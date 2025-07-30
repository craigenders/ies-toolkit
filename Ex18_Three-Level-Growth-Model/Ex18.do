// Three-Level Regression with a Cross-Level Interaction
clear

// read stacked data
infile imp school student wave condition teachexp eslpct ethnic male frlunch lowach stanmath month0 month7 probsolve efficacy using "~/desktop/imps0.dat"

// create unique row id within each data set
generate rownum = student*100 + wave

// recode missing data in original data (imp = 0)
recode condition - efficacy (999 = .)

// convert to mi data
mi import flong, m(imp) id(rownum) imputed(condition - efficacy) clear

// center variables
summarize male, meanonly
gen malecent = male - r(mean)

summarize frlunch, meanonly
gen frlunchcent = frlunch - r(mean)

summarize teachexp, meanonly
gen teachexpcent = teachexp - r(mean)

// analysis and pooling
mi estimate, cmdok: mixed probsolve month7 malecent frlunchcent teachexpcent condition c.month7#c.condition || school: month7, covariance(unstructured) || student: month7, covariance(unstructured)
