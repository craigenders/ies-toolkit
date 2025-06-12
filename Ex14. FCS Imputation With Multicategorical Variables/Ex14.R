##################################################
# fcs imputation for multicategorical predictors
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
# fcs imputation (ex14.imp)
##################################################

mymodel <- rblimp_fcs(
  data = behaviorachievement,
  nominal = 'riskgrp',
  variables = 'read9 read1 lrnprob1 behsymp1 riskgrp stanread7 read2',
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

# pooled r-square
rsquare <- mean(unlist(lapply(implist, (function(x) summary(lm(read9 ~ read1 + lrnprob1 + behsymp1 + factor(riskgrp), data = x))$r.squared))))
rsquare

##################################################
# wald test of null model
##################################################

null <- with(implist, lm(read9 ~ 1))
testModels(fit, null, df.com = 132, method = "D1")

