##################################################
# mcmc logistic regression
##################################################

library(rblimp)
library(mitml)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/behaviorachievement.rda"
load(gzcon(url(data_url, open = "rb")))

##################################################
# fit model with mcmc (ex7.1.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  ordinal = 'readgrp9 ',
  model = '  
   focal.model:
   logit(readgrp9) ~ read1@beta1 lrnprob1@beta2 behsymp1@beta3;
   auxiliary.models:
   stanread7 read2  ~ readgrp9 read1 lrnprob1 behsymp1',
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
  ordinal = 'readgrp9 ',
  model = '  
   focal.model:
   logit(readgrp9) ~ read1@beta1 lrnprob1@beta2 behsymp1@beta3;
   auxiliary.models:
   stanread7 read2  ~ readgrp9 read1 lrnprob1 behsymp1',
  waldtest = 'beta1:beta3 = 0',
  seed = 90291,
  burn = 1000,
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
fit <- with(implist, glm(readgrp9 ~ read1 + lrnprob1 + behsymp1, family = 'binomial'))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, df.com = 134)
estimates
confint(estimates) # confidence intervals

##################################################
# wald test of null model
##################################################

null <- with(implist, glm(readgrp9 ~ 1, family = 'binomial'))
testModels(fit, null, df.com = 134, method = 'D1')
