// Two-Level Regression With Random Intercepts
clear

// read stacked data
infile imp school student condition teachexp eslpct hispanic male frlunch lowach stanmath efficacypre efficacypst psolvepre psolvepst using "~/desktop/imps0.dat"

// recode missing data in original data (imp = 0)
recode condition - psolvepst (999 = .)

// convert to mi data
mi import flong, m(imp) id(student) imputed(condition - psolvepst) clear

// compute group means
egen psolvepre_meanj = mean(psolvepre), by(imp school)
egen hispanic_meanj = mean(hispanic), by(imp school)
egen frlunch_meanj = mean(frlunch), by(imp school)

// center predictors
gen psolvepre_cwc = psolvepre - psolvepre_meanj
gen hispanic_cwc = hispanic - hispanic_meanj
gen frlunch_cwc = frlunch - frlunch_meanj
summarize psolvepre_meanj, meanonly
gen psolvepre_meanjcgm = psolvepre_meanj - r(mean)
summarize hispanic_meanj, meanonly
gen hispanic_meanjcgm = hispanic_meanj - r(mean)
summarize frlunch_meanj, meanonly
gen frlunch_meanjcgm = frlunch_meanj - r(mean)

// analysis and pooling
mi estimate, cmdok: mixed psolvepst psolvepre_cwc hispanic_cwc frlunch_cwc psolvepre_meanjcgm hispanic_meanjcgm frlunch_meanjcgm condition || school:, cov(id)  var
