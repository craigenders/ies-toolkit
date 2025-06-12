##################################################
# mcmc regression w multicategorical predictor
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
# fit model with mcmc (ex9.1.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  nominal = 'riskgrp',
  model = '  
   focal.model:
   read9 ~ read1 lrnprob1 behsymp1 riskgrp;
   auxiliary.models:
   stanread7 read2   ~ read9 read1 lrnprob1 behsymp1 riskgrp',
  seed = 90291,
  burn = 2000,
  iter = 10000)
output(mymodel)

##################################################
# fit model and save imputations (ex9.2.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  nominal = 'riskgrp',
  model = '  
   focal.model:
   read9 ~ read1 lrnprob1 behsymp1 riskgrp;
   auxiliary.models:
   stanread7 read2   ~ read9 read1 lrnprob1 behsymp1 riskgrp',
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
fit <- with(implist, lm(read9 ~ read1 + lrnprob1 + behsymp1 + factor(riskgrp)))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, extra.pars = T, df.com = 132)
estimates
confint(estimates) # confidence intervals

##################################################
# wald test of null model
##################################################

null <- with(implist, lm(read9 ~ 1))
testModels(fit, null, df.com = 132, method = 'D1')

