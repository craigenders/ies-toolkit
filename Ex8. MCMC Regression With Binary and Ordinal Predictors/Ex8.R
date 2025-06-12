##################################################
# mcmc regression w binary and ordinal predictors
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
# fit model with mcmc (ex8.1.imp)
##################################################

mymodel <- rblimp(
  data = mathachievement,
  ordinal = 'condition frlunch atrisk efficacy',
  fixed = 'condition mathpre',
  model = ' 
   focal.model:
   mathpost ~ condition@beta1 frlunch@beta2 efficacy@beta3 mathpre@beta4;
   auxiliary.models:
   stanread atrisk ~ mathpost condition frlunch efficacy mathpre',
  waldtest = 'beta1:beta4 = 0',
  seed = 90291,
  burn = 5000,
  iter = 10000)
output(mymodel)

##################################################
# fit model and save imputations (ex8.2.imp)
##################################################

mymodel <- rblimp(
  data = mathachievement,
  ordinal = 'condition frlunch atrisk efficacy',
  fixed = 'condition mathpre',
  model = ' 
   focal.model:
   mathpost ~ condition@beta1 frlunch@beta2 efficacy@beta3 mathpre@beta4;
   auxiliary.models:
   stanread atrisk ~ mathpost condition frlunch efficacy mathpre',
  waldtest = 'beta1:beta4 = 0',
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

# analysis
fit <- with(implist, lm(mathpost ~ condition + frlunch + efficacy + mathpre))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, extra.pars = T, df.com = 245)
estimates
confint(estimates) # confidence intervals

# pooled r-square
rsquare <- mean(unlist(lapply(implist, (function(x) summary(lm(mathpost ~ condition + frlunch + efficacy + mathpre, data = x))$r.squared))))
rsquare

##################################################
# wald test of null model
##################################################

null <- with(implist, lm(mathpost ~ 1))
testModels(fit, null, df.com = 245, method = 'D1')


