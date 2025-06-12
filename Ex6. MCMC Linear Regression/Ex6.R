##################################################
# mcmc linear regression
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
# fit model with mcmc (ex6.1.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  model = '  
   focal.model:
   read9 ~ read1@beta1 lrnprob1@beta2 behsymp1@beta3;
   auxiliary.models:
   stanread7 read2  ~ read9 read1 lrnprob1 behsymp1',
  waldtest = 'beta1:beta3 = 0',
  seed = 90291,
  burn = 1000,
  iter = 10000)
mymodel@output

##################################################
# fit model and save imputations (ex6.2.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  model = '  
   focal.model:
   read9 ~ read1@beta1 lrnprob1@beta2 behsymp1@beta3;
   auxiliary.models:
   stanread7 read2  ~ read9 read1 lrnprob1 behsymp1',
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

# analysis and pooling
fit <- with(implist, lm(read9 ~ read1 + lrnprob1 + behsymp1))

# significance tests with barnard & rubin degrees of freedom
estimates <- testEstimates(fit, extra.pars = T, df.com = 134)
estimates
confint(estimates)  # confidence intervals

##################################################
# wald test of null model
##################################################

null <- with(implist, lm(read9 ~ 1))
testModels(fit, null, df.com = 134, method = 'D1')
