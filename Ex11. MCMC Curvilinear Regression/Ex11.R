##################################################
# mcmc curvilinear regression
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
# fit model with mcmc (ex11.1.imp)
##################################################

mymodel <- rblimp(
  data = mathachievement,
  ordinal = 'atrisk frlunch efficacy',
  fixed = 'mathpre',
  center = 'anxiety',
  model = ' 
   focal.model:
   mathpost ~ anxiety anxiety^2@beta2 frlunch efficacy mathpre;
   auxiliary.models:
   stanread atrisk ~ mathpost anxiety frlunch efficacy mathpre',
  seed = 90291,
  burn = 10000,
  iter = 10000)
output(mymodel)

##################################################
# fit model and save imputations (ex11.2.imp)
##################################################

mymodel <- rblimp(
  data = mathachievement,
  ordinal = 'atrisk frlunch efficacy',
  fixed = 'mathpre',
  center = 'anxiety',
  model = ' 
   focal.model:
   mathpost ~ anxiety anxiety^2@beta2 frlunch efficacy mathpre;
   auxiliary.models:
   stanread atrisk ~ mathpost anxiety frlunch efficacy mathpre',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20,
  chains = 20)
output(mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# center lower-order variable and compute the square
mean_anxiety <- mean(unlist(lapply(implist, function(df) mean(df$anxiety))))
for (i in 1:length(implist)) {
  implist[[i]]$anxiety.cgm <- implist[[i]]$anxiety - mean_anxiety
}

# analysis
fit <- with(implist, lm(mathpost ~ anxiety.cgm + I(anxiety.cgm^2) + frlunch  + efficacy + mathpre))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, extra.pars = T, df.com = 244)
estimates
confint(estimates) # confidence intervals

# pooled r-square
rsquare <- mean(unlist(lapply(implist, (function(x) summary(lm(mathpost ~ anxiety.cgm + anxiety.sq + frlunch  + efficacy + mathpre, data = x))$r.squared))))
rsquare
