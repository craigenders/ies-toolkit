##################################################
# fcs imputation for linear regression
##################################################

library(fdir)
library(rblimp)
library(mitml)

##################################################
# set working directory and load data
##################################################

set()
load('behaviorachievement.rda')

##################################################
# fcs imputation (ex13.imp)
##################################################

mymodel <- rblimp_fcs(
  data = behaviorachievement,
  variables = 'read9 read1 lrnprob1 behsymp1 stanread7 read2',
  seed = 90291,
  burn = 2000,
  iter = 10000,
  nimps = 20,
  chains = 20)
output(mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# analysis
fit <- with(implist, lm(read9 ~ read1 + lrnprob1 + behsymp1))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, extra.pars = T, df.com = 134)
estimates
confint(estimates) # confidence intervals

# pooled r-square
rsquare <- mean(unlist(lapply(implist, (function(x) summary(lm(read9 ~ read1 + lrnprob1 + behsymp1, data = x))$r.squared))))
rsquare

##################################################
# wald and likelihood ratio (omnibus) tests
##################################################

# wald test that all slopes = 0
null <- with(implist, lm(read9 ~ 1))
testModels(fit, null, df.com = 134, method = "D1")
