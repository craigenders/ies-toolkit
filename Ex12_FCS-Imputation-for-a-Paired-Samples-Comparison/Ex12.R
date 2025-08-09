##################################################
# fcs imputation for paired t-test
##################################################

library(rblimp)
library(mitml)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/mathachievement.rda"
load(gzcon(url(data_url, open = "rb")))

##################################################
# fcs imputation (ex12.imp)
##################################################

mymodel <- rblimp_fcs(
  data = mathachievement,
  ordinal = 'frlunch efficacy',
  fixed = 'mathpre',
  variables = 'mathpost mathpre frlunch stanread efficacy',
  seed = 90291,
  burn = 5000,
  iter = 10000,
  nimps = 20,
  chains = 20)
output(mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# compute change score from imputed data
for (i in 1:length(implist)) {
  implist[[i]]$change <- implist[[i]]$mathpost -  implist[[i]]$mathpre
}

# analysis
fit <- with(implist, lm(change ~ 1))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, extra.pars = T, df.com = 249)
estimates
confint(estimates) # confidence intervals
