##################################################
# fcs imputation for paired t-test
##################################################

library(fdir)
library(rblimp)
library(mitml)

##################################################
# set working directory and load data
##################################################

set()
load('mathachievement.rda')

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
