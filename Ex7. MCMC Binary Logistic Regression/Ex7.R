##################################################
# mcmc logistic regression
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
# fit model with mcmc (ex7.1.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  ordinal = 'read9grp ',
  model = '  
   focal.model:
   logit(read9grp) ~ read1@beta1 lrnprob1@beta2 behsymp1@beta3;
   auxiliary.models:
   stanread7 read2  ~ read9grp read1 lrnprob1 behsymp1',
  waldtest = 'beta1:beta3 = 0',
  seed = 90291,
  burn = 1000,
  iter = 10000)
output(mymodel)

##################################################
# fit model and save imputations (ex7.2.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  ordinal = 'read9grp ',
  model = '  
   focal.model:
   logit(read9grp) ~ read1@beta1 lrnprob1@beta2 behsymp1@beta3;
   auxiliary.models:
   stanread7 read2  ~ read9grp read1 lrnprob1 behsymp1',
  waldtest = 'beta1:beta3 = 0',
  seed = 90291,
  burn = 1000,
  iter = 10000,
  nimps = 20,
  chains = 20)
mymodel@output

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# analysis
fit <- with(implist, glm(read9grp ~ read1 + lrnprob1 + behsymp1, family = 'binomial'))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, df.com = 134)
estimates
confint(estimates) # confidence intervals

##################################################
# wald test of null model
##################################################

null <- with(implist, glm(read9grp ~ 1, family = 'binomial'))
testModels(fit, null, df.com = 134, method = 'D1')
